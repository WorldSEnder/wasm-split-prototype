use proc_macro::{Span, TokenStream};

use quote::{format_ident, quote, quote_spanned};
use sha2::Digest;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, parse_quote, Attribute, Block, Pat, Path, ReturnType};
use syn::{parse_macro_input, spanned::Spanned, Ident, ItemFn, LitStr, Signature, Token};

mod magic_constants;
use magic_constants::PLACEHOLDER_IMPORT_MODULE;

struct ReturnWrapper {
    pattern: Pat,
    output: ReturnType,
    postlude: Block,
}

struct PreloadDefinition {
    attrs: Vec<Attribute>,
    name: Ident,
}

struct Args {
    module_ident: Ident,
    link_name: Option<(Ident, LitStr)>,
    wasm_split_path: Option<Path>,
    return_wrapper: Option<ReturnWrapper>,
    preload_def: Option<PreloadDefinition>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let module_ident = input.call(Ident::parse_any)?;
        let mut link_name = None;
        let mut wasm_split_path = None;
        let mut return_wrapper = None;
        let mut preload_def = None;
        while !input.is_empty() {
            let _: Token![,] = input.parse()?;
            if input.is_empty() {
                break;
            }
            let option: Ident = input.call(Ident::parse_any)?;
            match () {
                _ if option == "wasm_import_module" => {
                    let _: Token![=] = input.parse()?;
                    link_name = Some((option, input.parse()?));
                }
                _ if option == "wasm_split_path" => {
                    let _: Token![=] = input.parse()?;
                    wasm_split_path = Some(input.parse()?);
                }
                _ if option == "return_wrapper" => {
                    let wrap_spec;
                    let _parens = parenthesized!(wrap_spec in input);
                    let _: Token![let] = wrap_spec.parse()?;
                    let pattern = Pat::parse_multi_with_leading_vert(&wrap_spec)?;
                    let _: Token![=] = wrap_spec.parse()?;
                    let _: Token![_] = wrap_spec.parse()?;
                    let _: Token![;] = wrap_spec.parse()?;
                    return_wrapper = Some(ReturnWrapper {
                        pattern,
                        postlude: wrap_spec.parse()?,
                        output: wrap_spec.parse()?,
                    });
                }
                _ if option == "preload" => {
                    let wrap_spec;
                    let _parens = parenthesized!(wrap_spec in input);
                    let attrs = wrap_spec.call(Attribute::parse_outer)?;
                    let name = wrap_spec.parse()?;
                    preload_def = Some(PreloadDefinition { attrs, name });
                }
                _ => {
                    return Err(syn::Error::new(
                        option.span(),
                        "No such option for the `split` macro.",
                    ))
                }
            }
        }
        Ok(Self {
            module_ident,
            link_name,
            wasm_split_path,
            return_wrapper,
            preload_def,
        })
    }
}

/// Indicate a function as a split point.
///
/// Calls to this function will load the module to which this function was split into on-demand. On non-`wasm` targets,
/// the function will be called directly without any split support.
///
/// The annotated function must fulfill the requirements a typical `extern` declared function must fufill:
/// - It can not be `async`. If you want to support this, you must `Box` or otherwise wrap the `Future` into a `dyn` object.
///   Also see the `return_wrapper` option for some further hints.
/// - It can not be `const`.
/// - It can not make use of a receiver argument, generics or an `impl` return type.
/// - By default, `"Rust"` is assumed as the ABI, but you can change this by declaring the function as `extern "ABI"`.
///
/// ## Syntax
///
/// ```text
/// wasm_split($module:ident (, $option ),* ) => { ... };
/// ```
///
/// The following options are supported:
/// - `wasm_split_path = $this:path` changes the path at which the runtime support crate is expected.
///    As a framework, you might want to reexport this from some hidden module path.
///    Default: `::wasm_split_helpers`.
/// - `return_wrapper( let $bindings:pat = _ ; $compute:block -> $ret:ty )`. A rather low-level option to support
///    rewriting the result of the wrapped function. The generated wrapper will, rather than directly return the result
///    from the user-given function, bind this to `$bindings` and emit the statements in `$compute` to generate the
///    return value of the wrapper with the return type indicated by `$ret`.
///
///    Example use case: `return_wrapper( let future = _ ; { future.await } -> Output)` to `await` a future directly in
///    the wrapper.
/// - `preload( $( #[$attr] )* $preload_name:ident )` generates an additional preload function `$preload_name` with the
///    signature `async fn()` which can be used to fetch the module in which the wrapped function is contained before
///    calling it directly.
#[proc_macro_attribute]
pub fn wasm_split(args: TokenStream, input: TokenStream) -> TokenStream {
    let Args {
        module_ident,
        link_name,
        wasm_split_path,
        return_wrapper,
        preload_def,
    } = parse_macro_input!(args as Args);
    let (deprecated_link_opt, link_name) = if let Some((option, link_name)) = link_name {
        (Some(option), link_name)
    } else {
        (
            None,
            LitStr::new(PLACEHOLDER_IMPORT_MODULE, Span::call_site().into()),
        )
    };
    let wasm_split_path = wasm_split_path.unwrap_or(parse_quote!(::wasm_split_helpers));

    let mut item_fn: ItemFn = parse_macro_input!(input as ItemFn);
    let mut declared_abi = item_fn.sig.abi.take();
    declared_abi.get_or_insert(parse_quote!( extern "Rust" ));
    let declared_async = item_fn.sig.asyncness.take();

    let mut wrapper_sig = Signature {
        asyncness: Some(Default::default()),
        ..item_fn.sig.clone()
    };

    if let Some(not_sync) = declared_async {
        return quote_spanned! {not_sync.span()=>
            ::core::compile_error!("Split functions can not be `async`");

            #wrapper_sig {
                ::core::todo!()
            }
        }
        .into();
    }

    let name = &item_fn.sig.ident;
    let PreloadDefinition {
        attrs: preload_attrs,
        name: preload_name,
    } = preload_def.unwrap_or_else(|| PreloadDefinition {
        attrs: vec![
            parse_quote!(#[automatically_derived]),
            parse_quote!(#[doc(hidden)]),
        ],
        name: format_ident!("__wasm_split_preload_{name}"),
    });
    let vis = item_fn.vis;

    let unique_identifier = base16::encode_lower(
        &sha2::Sha256::digest(format!("{name} {span:?}", span = name.span()))[..16],
    );

    let load_module_ident = format_ident!("__wasm_split_load_{module_ident}");
    let impl_import_ident =
        format_ident!("__wasm_split_00{module_ident}00_import_{unique_identifier}_{name}");
    let impl_export_ident =
        format_ident!("__wasm_split_00{module_ident}00_export_{unique_identifier}_{name}");

    let export_sig = Signature {
        abi: declared_abi.clone(),
        ident: impl_export_ident.clone(),
        ..item_fn.sig.clone()
    };

    let mut args = Vec::new();
    for (i, param) in wrapper_sig.inputs.iter_mut().enumerate() {
        match param {
            syn::FnArg::Typed(pat_type) => {
                let param_ident = format_ident!("__wasm_split_arg_{i}");
                args.push(param_ident.clone());
                pat_type.pat = Box::new(syn::Pat::Ident(syn::PatIdent {
                    attrs: vec![],
                    by_ref: None,
                    mutability: None,
                    ident: param_ident,
                    subpat: None,
                }));
            }
            // receiver arguments can not used in `extern` functions (and we can't name the `Self` type in the arguments to work arount that)
            syn::FnArg::Receiver(_) => {
                return quote_spanned! {param.span()=>
                    ::core::compile_error!("Split functions can not have a receiver argument");

                    #wrapper_sig {
                        ::core::todo!()
                    }
                }
                .into();
            }
        }
    }
    let import_sig = Signature {
        //abi: declared_abi.clone(), // already in an extern block
        asyncness: None,
        ident: impl_import_ident.clone(),
        ..wrapper_sig.clone()
    };

    let attrs = item_fn.attrs;
    let stmts = &item_fn.block.stmts;

    let mut compute_result = quote! {
        #[cfg(target_family = "wasm")]
        use #impl_import_ident as callee;
        #[cfg(not(target_family = "wasm"))]
        use #impl_export_ident as callee;
        callee( #(#args),* )
    };

    if let Some(ReturnWrapper {
        output,
        pattern: output_pat,
        postlude,
    }) = return_wrapper
    {
        wrapper_sig.output = output;
        let postlude = postlude.stmts;
        compute_result = quote! {{
            let #output_pat = { #compute_result };
            #( #postlude )*
        }};
    }

    let mut extra_code = quote! {};
    if let Some(deprecated_opt) = deprecated_link_opt {
        let deprecation_note = format!("The `{deprecated_opt}` option should not be used, since the wasm_split_cli fixes the import path with improved target knowledge.");
        extra_code.extend(quote! {
            const _: () = {
                #[allow(nonstandard_style)]
                #[deprecated(note = #deprecation_note)]
                const #deprecated_opt: () = ();
                let _ = #deprecated_opt;
            };
        });
    }

    quote! {
        // This could have weak linkage to unify all mentions of the same module
        #( #preload_attrs )*
        #vis async fn #preload_name () {
            #[cfg(target_family = "wasm")]
            #[link(wasm_import_module = #link_name)]
            unsafe extern "C" {
                #[unsafe(no_mangle)]
                fn #load_module_ident (callback: #wasm_split_path::rt::LoadCallbackFn, data: *const ::std::ffi::c_void) -> ();
            }
            #[cfg(target_family = "wasm")]
            {
                #wasm_split_path::rt::ensure_loaded(::core::pin::Pin::static_ref({
                    // SAFETY: the imported c function correctly implements the callback
                    static LOADER: #wasm_split_path::rt::LazySplitLoader = unsafe { #wasm_split_path::rt::LazySplitLoader::new(#load_module_ident) };
                    &LOADER
                })).await;
            }
        }
        #(#attrs)*
        #vis #wrapper_sig {
            #[cfg(target_family = "wasm")]
            #[link(wasm_import_module = #link_name)]
            unsafe #declared_abi {
                // We rewrite calls to this function instead of actually calling it. We just need to link to it. The name is unique by hashing.
                #[unsafe(no_mangle)]
                safe #import_sig;
            }

            #(#attrs)*
            #[cfg_attr(target_family = "wasm", unsafe(no_mangle))]
            #export_sig {
                #(#stmts)*
            }

            #preload_name ().await;
            #compute_result
        }
        #extra_code
    }
    .into()
}
