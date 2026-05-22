#' Number of embedding dimensions
#'
#' The dimensionality of the embedding space for features in attention-based
#' tabular models.
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
#' num_embedding()
#' num_embedding(range = c(8L, 32L))
#'
#' @export
num_embedding <- function(range = c(8L, 64L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(num_embedding = "# Embedding Dimensions"),
    finalize = NULL
  )
}
