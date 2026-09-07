use tracing::{field, span::Entered, Span};

pub struct TraceCtx {
    span: Option<Span>,
}
pub struct Guard<'a> {
    inner: Option<Entered<'a>>,
}
impl TraceCtx {
    pub fn new(span: Option<Span>) -> Self {
        Self { span }
    }
    pub fn enter(&self) -> Guard<'_> {
        Guard {
            inner: self.span.as_ref().map(|span| span.enter()),
        }
    }
    pub fn record<Q: field::AsField + ?Sized, V: field::Value>(&self, field: &Q, value: V) {
        if let Some(span) = self.span.as_ref() {
            span.record(field, value);
        }
    }
}
impl Guard<'_> {
    pub fn exit(self) {
        let _ = self.inner;
    }
}

macro_rules! perf_span {
    ($name:expr $(, $($fields:tt)* )?) => {
        $crate::tracing_support::TraceCtx::new(
            if ::tracing::span_enabled!(::tracing::Level::ERROR, perf_key = $name, $( $($fields)* )? ) {
                Some(::tracing::span!(::tracing::Level::TRACE, $name, perf_key = $name, $( $($fields)* )? ))
            } else {
                None
            }
        )
    };
}
pub(crate) use perf_span;
