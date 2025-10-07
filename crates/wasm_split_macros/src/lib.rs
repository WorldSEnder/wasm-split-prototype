use proc_macro::{Span, TokenStream};

use quote::{format_ident, quote, quote_spanned};
use sha2::Digest;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, parse_quote, Block, Pat, Path, ReturnType};
use syn::{parse_macro_input, spanned::Spanned, Ident, ItemFn, LitStr, Signature, Token};

struct ReturnWrapper {
    pattern: Pat,
    output: ReturnType,
    postlude: Block,
}

struct Args {
    module_ident: Ident,
    link_name: LitStr,
    wasm_split_path: Path,
    return_wrapper: Option<ReturnWrapper>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let module_ident = input.call(Ident::parse_any)?;
        let mut link_name: Option<LitStr> = None;
        let mut wasm_split_path: Option<Path> = None;
        let mut return_wrapper: Option<ReturnWrapper> = None;
        while !input.is_empty() {
            let _: Token![,] = input.parse()?;
            if input.is_empty() {
                break;
            }
            let option: Ident = input.call(Ident::parse_any)?;
            match () {
                _ if option == "wasm_import_module" => {
                    let _: Token![=] = input.parse()?;
                    link_name = Some(input.parse()?);
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
            link_name: link_name
                .unwrap_or(LitStr::new("./__wasm_split.js", Span::call_site().into())),
            wasm_split_path: wasm_split_path.unwrap_or(parse_quote!(::wasm_split)),
            return_wrapper,
        })
    }
}

#[proc_macro_attribute]
pub fn wasm_split(args: TokenStream, input: TokenStream) -> TokenStream {
    let Args {
        module_ident,
        link_name,
        wasm_split_path,
        return_wrapper,
    } = parse_macro_input!(args as Args);
    let mut item_fn: ItemFn = parse_macro_input!(input as ItemFn);
    let mut declared_abi = item_fn.sig.abi.take();
    declared_abi.get_or_insert(syn::Abi {
        extern_token: Default::default(),
        name: Some(syn::LitStr::new("Rust", Span::mixed_site().into())),
    });
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
    let preload_name = Ident::new("preload", name.span());

    let unique_identifier = base16::encode_lower(
        &sha2::Sha256::digest(format!("{name} {span:?}", span = name.span()))[..16],
    );

    let load_module_ident = format_ident!("__wasm_split_load_{module_ident}");
    let impl_import_ident =
        format_ident!("__wasm_split_00{module_ident}00_import_{unique_identifier}_{name}");
    let impl_export_ident =
        format_ident!("__wasm_split_00{module_ident}00_export_{unique_identifier}_{name}");

    let import_sig = Signature {
        //abi: declared_abi.clone(), // already in an extern block
        ident: impl_import_ident.clone(),
        ..item_fn.sig.clone()
    };
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
            syn::FnArg::Receiver(_) => {
                args.push(format_ident!("self"));
            }
        }
    }

    let attrs = item_fn.attrs;
    let stmts = &item_fn.block.stmts;

    let mut call_input_fn = quote! {
        #impl_import_ident( #(#args),* )
    };

    if let Some(ReturnWrapper {
        output,
        pattern: output_pat,
        postlude,
    }) = return_wrapper
    {
        wrapper_sig.output = output;
        let postlude = postlude.stmts;
        call_input_fn = quote! {{
            let #output_pat = #call_input_fn;
            #( #postlude )*
        }};
    }

    quote! {
        #[doc(hidden)]
        mod #name {
            #[link(wasm_import_module = #link_name)]
            unsafe extern "C" {
                #[unsafe(no_mangle)]
                fn #load_module_ident (callback: #wasm_split_path ::LoadCallbackFn, data: *const ::std::ffi::c_void) -> ();
            }

            // This could have weak linkage to unify all mentions of the same module
            pub async fn #preload_name () {
                #wasm_split_path::ensure_loaded(::core::pin::Pin::static_ref({
                    // SAFETY: the imported c function correctly implements the callback
                    static LOADER: #wasm_split_path::LazySplitLoader = unsafe { #wasm_split_path::LazySplitLoader::new(#load_module_ident) };
                    &LOADER
                })).await;
            }
        }
        #wrapper_sig {
            #[link(wasm_import_module = #link_name)]
            unsafe #declared_abi {
                // We rewrite calls to this function instead of actually calling it. We just need to link to it. The name is unique by hashing.
                #[unsafe(no_mangle)]
                safe #import_sig;
            }

            #(#attrs)*
            #[no_mangle]
            #export_sig {
                #(#stmts)*
            }

            #name :: #preload_name ().await;
            #call_input_fn
        }
    }
    .into()
}
