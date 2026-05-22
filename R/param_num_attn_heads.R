#' Number of attention heads
#'
#' The number of parallel attention mechanisms (heads) in the multi-head
#' attention layer.
#'
#' @param range A two-element vector with the lower and upper bounds.
#' @param trans A transformation object (default `NULL` for no transformation).
#'
#' @details
#' Used as a tuning parameter for [tabular_auto_int()] when fit with the
#' `"brulee"` engine. Multiple attention heads allow the model to attend to
#' different aspects of the input features simultaneously.
#'
#' @return A `param` object.
#'
#' @examples
#' num_attn_heads()
#' num_attn_heads(range = c(1L, 4L))
#'
#' @export
num_attn_heads <- function(range = c(1L, 8L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_attn_heads = "# Attention Heads"),
    finalize = NULL
  )
}
