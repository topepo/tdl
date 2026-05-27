#' Number of attention features
#'
#' The dimensionality of the feature space used in the attention mechanism
#' of tabular models.
#'
#' @param range A two-element vector with the lower and upper bounds.
#' @param trans A transformation object (default `NULL` for no transformation).
#'
#' @details
#' Used as a tuning parameter for [tabular_auto_int()] when fit with the
#' `"brulee"` engine.
#'
#' @return A `param` object.
#'
#' @examples
#' num_attn_feat()
#' num_attn_feat(range = c(8L, 32L))
#'
#' @export
num_attn_feat <- function(range = c(8L, 64L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_attn_feat = "# Attention Features"),
    finalize = NULL
  )
}
