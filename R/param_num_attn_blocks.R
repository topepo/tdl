#' Number of attention blocks
#'
#' The number of sequential attention blocks in the model architecture.
#'
#' @param range A two-element vector with the lower and upper bounds.
#' @param trans A transformation object (default `NULL` for no transformation).
#'
#' @details
#' Used as a tuning parameter for [tabular_auto_int()] when fit with the
#' `"brulee"` engine. Each attention block consists of a multi-head attention
#' layer followed by feed-forward layers.
#'
#' @return A `param` object.
#'
#' @examples
#' num_attn_blocks()
#' num_attn_blocks(range = c(1L, 4L))
#'
#' @export
num_attn_blocks <- function(range = c(1L, 6L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_attn_blocks = "# Attention Blocks"),
    finalize = NULL
  )
}
