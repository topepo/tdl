#' TODO: test more esp with new_data = NULL

#' Chronos for Tabular Quantile Regression
#'
#' @description
#' `tabular_chronos()` uses Amazon's Chronos foundation model adapted for
#' tabular data with quantile regression.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tabular_chronos")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character value for the type of model.
#'  The only possible value for this model is "quantile regression".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. The default for this model is `"brulee"`.
#'
#' @templateVar modeltype tabular_chronos
#'
#' @details This function fits quantile regression models.
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tabular_chronos")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tabular_chronos")
#'
#' tabular_chronos()
#' @export
tabular_chronos <-
  function(
    mode = "quantile regression",
    engine = "brulee",
    prediction_length = 10
  ) {
    parsnip::new_model_spec(
      "tabular_chronos",
      args = list(prediction_length = prediction_length),
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update tabular_chronos
#' @rdname tdl_update
#' @export
update.tabular_chronos <-
  function(
    object,
    prediction_length = NULL,
    parameters = NULL,
    fresh = FALSE,
    ...
  ) {
    parsnip::update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = list(prediction_length = prediction_length),
      fresh = fresh,
      cls = "tabular_chronos",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tabular_chronos <- function(object, call = rlang::caller_env()) {
  invisible(object)
}

#' @export
required_pkgs.tabular_chronos <- function(x, infra = TRUE, ...) {
  c("brulee", "tdl")
}

# ------------------------------------------------------------------------------

make_tabular_chronos <- function() {
  parsnip::set_new_model("tabular_chronos")
  parsnip::set_model_mode("tabular_chronos", mode = "quantile regression")

  parsnip::set_model_engine(
    "tabular_chronos",
    mode = "quantile regression",
    eng = "brulee"
  )
  parsnip::set_dependency(
    "tabular_chronos",
    eng = "brulee",
    pkg = "brulee",
    mode = "quantile regression"
  )

  parsnip::set_model_arg(
    model = "tabular_chronos",
    eng = "brulee",
    parsnip = "prediction_length",
    original = "prediction_length",
    func = list(
      pkg = "dials",
      fun = "prediction_length",
      range = c(1, 1024)
    ),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "quantile regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_chronos"),
      defaults = list(quantile_levels = expr(quantile_levels))
    )
  )

  parsnip::set_encoding(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "quantile regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tabular_chronos",
    eng = "brulee",
    mode = "quantile regression",
    type = "quantile",
    value = list(
      pre = NULL,
      post = parsnip::matrix_to_quantile_pred,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "quantile"
      )
    )
  )
}
