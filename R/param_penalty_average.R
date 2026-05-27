#' Target geometric mean of per-weight regularization coefficients
#'
#' The target geometric mean of the per-weight regularization coefficients
#' (Theta in Shavitt and Segal (2018)) for Regularization Learning Networks.
#' Best tuned on the log10 scale.
#'
#' @param range A two-element vector with the lower and upper bounds in log10
#'   scale (default `c(-15, -5)`, corresponding to `1e-15` to `1e-5`).
#' @param trans A transformation object. Defaults to log10.
#'
#' @details
#' Used as a tuning parameter for [tabular_rln()] when fit with the `"brulee"` engine.
#' The value is passed to `brulee::brulee_rln()` on the natural scale.
#'
#' @return A `param` object.
#'
#' @references
#' Shavitt, I., & Segal, E. (2018). Regularization learning networks: Deep
#' learning for tabular datasets. _Advances in Neural Information Processing
#' Systems_, 31, 1379-1389.
#'
#' @examples
#' penalty_average()
#' penalty_average(range = c(-12, -4))
#'
#' @export
penalty_average <- function(
  range = c(-15, -5),
  trans = scales::transform_log10()
) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(penalty_average = "Penalty Average"),
    finalize = NULL
  )
}
