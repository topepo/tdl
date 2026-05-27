#' Dropout rate for embeddings
#'
#' The proportion of embedding values to randomly set to zero during model
#' training.
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
#' dropout_embedding()
#' dropout_embedding(range = c(0, 0.3))
#'
#' @export
dropout_embedding <- function(range = c(0, 0.5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(dropout_embedding = "Embedding Dropout Rate"),
    finalize = NULL
  )
}
