use winnow::error::{ContextError, ParseError};

use crate::range::Range;

#[derive(Debug)]
pub struct ComRaTTError {
    pub message: String,
    pub span: Range,
}

impl ComRaTTError {
    pub fn new(message: String, span: Range) -> Self {
        Self { message, span }
    }
    pub fn from_parse(error: ParseError<&str, ContextError>, input: &str) -> Self {
        let message = error.inner().to_string();
        let input = input.to_owned();
        let start = error.offset();
        let end = (start + 1..)
            .find(|e| input.is_char_boundary(*e))
            .unwrap_or(start);

        Self {
            message,
            span: (start..end).into(),
        }
    }
}

impl std::fmt::Display for ComRaTTError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = annotate_snippets::Level::Error.title(&self.message);
        // .snippet(
        //     annotate_snippets::Snippet::source(&self.input)
        //         // .fold(true)
        //         .annotation(annotate_snippets::Level::Error.span(self.span.clone())),
        // );
        let renderer = annotate_snippets::Renderer::styled();
        let rendered = renderer.render(message);
        rendered.fmt(f)
    }
}
