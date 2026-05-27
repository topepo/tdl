#' Number of bottleneck units
#'
#' The number of units used in ResNet layers within a residual
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
#' bottleneck_units()
#' bottleneck_units(range = c(4L, 8L))
#'
#' @export
bottleneck_units <- function(range = c(2L, 25L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(bottleneck_units = "# Bottleneck Units"),
    finalize = NULL
  )
}
