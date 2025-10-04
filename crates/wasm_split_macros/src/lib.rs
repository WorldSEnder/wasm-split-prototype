use proc_macro::{Span, TokenStream};

use digest::Digest;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, Ident, ItemFn, Signature};

#[proc_macro_attribute]
pub fn wasm_split(args: TokenStream, input: TokenStream) -> TokenStream {
    let module_ident = parse_macro_input!(args as Ident);
    let mut item_fn = parse_macro_input!(input as ItemFn);
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

    let unique_identifier = base16::encode_lower(
        &sha2::Sha256::digest(format!("{name} {span:?}", span = name.span()))[..16],
    );

    let load_module_ident = format_ident!("__wasm_split_load_{module_ident}");
    let split_loader_ident = format_ident!("__wasm_split_loader_{module_ident}");
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

    quote! {
        #wrapper_sig {

            #[link(wasm_import_module = "./__wasm_split.js")]
            unsafe extern "C" {
                #[unsafe(no_mangle)]
                fn #load_module_ident (callback: ::wasm_split::LoadCallbackFn, data: *const ::std::ffi::c_void) -> ();
            }
            #[link(wasm_import_module = "./__wasm_split.js")]
            unsafe #declared_abi {
                // We rewrite calls to this function instead of actually calling it. We just need to link to it. The name is unique by hashing.
                #[unsafe(no_mangle)]
                safe #import_sig;
            }
            // This could have weak linkage to unify all mentions of the same module
            async fn #split_loader_ident () {
                ::wasm_split::ensure_loaded(::core::pin::Pin::static_ref({
                    // SAFETY: the imported c function correctly implements the callback
                    static LOADER: ::wasm_split::LazySplitLoader = unsafe { ::wasm_split::LazySplitLoader::new(#load_module_ident) };
                    &LOADER
                })).await;
            }

            #(#attrs)*
            #[no_mangle]
            #export_sig {
                #(#stmts)*
            }

            #split_loader_ident ().await;
            #impl_import_ident( #(#args),* )
        }
    }
    .into()
}
