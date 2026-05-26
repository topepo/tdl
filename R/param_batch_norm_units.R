#' Number of batch normalization units
#'
#' The number of units used in batch normalization layers within a residual
#' block.
#'
#' @param range A two-element vector with the lower and upper bounds.
#' @param trans A transformation object (default `NULL` for no transformation).
#'
#' @details
#' Used as a tuning parameter for [tabular_resnet()] when fit with the `"brulee"`
#' engine.
#'
#' @return A `param` object.
#'
#' @examples
#' batch_norm_units()
#' batch_norm_units(range = c(4L, 8L))
#'
#' @export
batch_norm_units <- function(range = c(2L, 25L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(batch_norm_units = "# BatchNorm Units"),
    finalize = NULL
  )
}
