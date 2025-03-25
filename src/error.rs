use crate::range::Range;

#[derive(Debug)]
pub struct ComRaTTError<'a> {
    pub message: String,
    pub input: &'a str,
    pub span: Range,
}

impl<'a> ComRaTTError<'a> {
    pub fn from_span(message: String, span: Range, input: &'a str) -> Self {
        Self {
            message,
            input,
            span,
        }
    }
}

impl std::fmt::Display for ComRaTTError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = annotate_snippets::Level::Error
            .title(&self.message)
            .snippet(
                annotate_snippets::Snippet::source(self.input)
                    .fold(true)
                    .annotation(annotate_snippets::Level::Error.span(self.span.into())),
            );
        let renderer = annotate_snippets::Renderer::styled();
        let rendered = renderer.render(message);
        rendered.fmt(f)
    }
}
