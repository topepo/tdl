#' Residual connection locations
#'
#' The layer indices at which residual connections are added in a tabular
#' residual network.
#'
#' @param range A two-element vector with the lower and upper bounds. The upper
#'   bound defaults to [dials::unknown()] and must be finalized before grid
#'   creation, e.g. with [update()].
#' @param trans A transformation object (default `NULL` for no transformation).
#'
#' @details
#' Used as a tuning parameter for [tab_resnet()] when fit with the `"brulee"`
#' engine. The upper bound depends on the number of hidden layers in the
#' network and so is left as `unknown()` by default.
#'
#' @return A `param` object.
#'
#' @examples
#' resid_at()
#' resid_at(range = c(2L, 6L))
#'
#' @export
resid_at <- function(range = c(2L, dials::unknown()), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(resid_at = "Residual Locations"),
    finalize = NULL
  )
}
