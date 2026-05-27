#' Regularization norm type
#'
#' The type of regularization norm applied to per-weight coefficients in
#' Regularization Learning Networks.
#'
#' @param values A character vector of possible values. Defaults to
#'   `c("L1", "L2")`.
#'
#' @details
#' Used as a tuning parameter for [tabular_rln()] when fit with the `"brulee"` engine.
#' L1 is recommended by the original paper.
#'
#' @return A `param` object.
#'
#' @examples
#' penalty_type()
#'
#' @export
penalty_type <- function(values = c("L1", "L2")) {
  dials::new_qual_param(
    type = "character",
    values = values,
    label = c(penalty_type = "Penalty Type"),
    finalize = NULL
  )
}
