#' Residual Neural Network for Tabular Data
#'
#' @description
#' `tab_resnet()` ... This function can fit classification and
#' regression models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("tab_resnet")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @inheritParams parsnip::mlp
#' @param hidden_units An integer vector for the number of units in the hidden
#' model.
#' @param penalty A non-negative numeric value for the amount of weight
#'  decay.
#' @param dropout A number between 0 (inclusive) and 1 denoting the proportion
#'  of model parameters randomly set to zero during model training.
#' @param epochs An integer for the number of training iterations.
#' @param activation A vector character strings denoting the type of relationship
#'  between the layers. The activation
#'  function between the hidden and output layers is automatically set to either
#'  "linear" or "softmax" depending on the type of outcome. Possible values
#'  depend on the engine being used.
#'
#' @templateVar modeltype tab_resnet
# @template spec-details
#'
# @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("tab_resnet")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("tab_resnet")
#'
#' tab_resnet(mode = "classification", penalty = 0.01)
#' @export

tab_resnet <-
  function(
    mode = "unknown",
    engine = "brulee",
    hidden_units = NULL,
    batch_norm_units = NULL,
    resid_at = NULL,
    penalty = NULL,
    dropout = NULL,
    epochs = NULL,
    activation = NULL,
    learn_rate = NULL
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      batch_norm_units = enquo(batch_norm_units),
      resid_at = enquo(resid_at),
      penalty = enquo(penalty),
      dropout = enquo(dropout),
      epochs = enquo(epochs),
      activation = enquo(activation),
      learn_rate = enquo(learn_rate)
    )

    parsnip::new_model_spec(
      "tab_resnet",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = !missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = !missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' Updating a model specification
#' @method update tab_resnet
#' @rdname tdl_update
#' @export
update.tab_resnet <-
  function(
    object,
    parameters = NULL,
    hidden_units = NULL,
    batch_norm_units = NULL,
    resid_at = NULL,
    penalty = NULL,
    dropout = NULL,
    epochs = NULL,
    activation = NULL,
    learn_rate = NULL,
    fresh = FALSE,
    ...
  ) {
    args <- list(
      hidden_units = enquo(hidden_units),
      batch_norm_units = enquo(batch_norm_units),
      resid_at = enquo(resid_at),
      penalty = enquo(penalty),
      dropout = enquo(dropout),
      epochs = enquo(epochs),
      activation = enquo(activation),
      learn_rate = enquo(learn_rate)
    )

    parsnip::update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "tab_resnet",
      ...
    )
  }

# ------------------------------------------------------------------------------

#' @export
check_args.tab_resnet <- function(object, call = rlang::caller_env()) {
  args <- lapply(object$args, rlang::eval_tidy)

  check_number_decimal(
    args$penalty,
    min = 0,
    allow_null = TRUE,
    call = call,
    arg = "penalty"
  )
  check_number_decimal(
    args$dropout,
    min = 0,
    max = 1,
    allow_null = TRUE,
    call = call,
    arg = "dropout"
  )

  if (
    is.numeric(args$penalty) &&
      is.numeric(args$dropout) &&
      args$dropout > 0 &&
      args$penalty > 0
  ) {
    cli::cli_abort(
      "Both weight decay and dropout should not be specified.",
      call = call
    )
  }

  invisible(object)
}

#' @export
required_pkgs.tab_resnet <- function(x, infra = TRUE, ...) {
  c("brulee", "tdl")
}

## -----------------------------------------------------------------------------

#' Model predictions across many sub-models
#' @importFrom purrr map
#' @importFrom dplyr arrange select
#' @rdname multi_predict
#' @inheritParams parsnip::multi_predict
#' @param epochs An integer vector for the number of training epochs.
#' @export
multi_predict._torch_resnet <-
  function(object, new_data, type = NULL, epochs = NULL, ...) {
    load_libs(object, quiet = TRUE, attach = TRUE)

    if (is.null(epochs)) {
      epochs <- length(object$fit$models)
    }

    epochs <- sort(epochs)

    if (is.null(type)) {
      if (object$spec$mode == "classification") {
        type <- "class"
      } else {
        type <- "numeric"
      }
    }

    res <-
      purrr::map(
        epochs,
        ~ predict(object, new_data, type, epochs = .x) |>
          dplyr::mutate(epochs = .x)
      ) |>
      purrr::map(\(x) x |> dplyr::mutate(.row = seq_len(nrow(new_data)))) |>
      purrr::list_rbind() |>
      dplyr::arrange(.row, epochs)
    res <- split(dplyr::select(res, -.row), res$.row)
    names(res) <- NULL
    tibble::tibble(.pred = res)
  }


reformat_torch_num <- function(results, object) {
  if (isTRUE(ncol(results) > 1)) {
    nms <- colnames(results)
    results <- tibble::as_tibble(results, .name_repair = "minimal")
    if (length(nms) == 0 && length(object$preproc$y_var) == ncol(results)) {
      names(results) <- object$preproc$y_var
    }
  } else {
    results <- unname(results[[1]])
  }
  results
}

# ------------------------------------------------------------------------------

make_tab_resnet <- function() {
  parsnip::set_new_model("tab_resnet")
  parsnip::set_model_mode("tab_resnet", mode = "classification")
  parsnip::set_model_mode("tab_resnet", mode = "regression")

  parsnip::set_model_engine(
    "tab_resnet",
    mode = "classification",
    eng = "brulee"
  )
  parsnip::set_model_engine("tab_resnet", mode = "regression", eng = "brulee")
  parsnip::set_dependency(
    "tab_resnet",
    eng = "brulee",
    pkg = "brulee",
    mode = "classification"
  )
  parsnip::set_dependency(
    "tab_resnet",
    eng = "brulee",
    pkg = "brulee",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "hidden_units",
    original = "hidden_units",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "batch_norm_units",
    original = "batch_norm_units",
    func = list(pkg = "tdl", fun = "batch_norm_units"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "penalty",
    original = "penalty",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "dropout",
    original = "dropout",
    func = list(pkg = "dials", fun = "dropout"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate", range = c(-2.5, -0.5)),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "activation",
    original = "activation",
    func = list(
      pkg = "dials",
      fun = "activation",
      values = c('relu', 'elu', 'tanh')
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "rate_schedule",
    original = "rate_schedule",
    func = list(
      pkg = "dials",
      fun = "rate_schedule"
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "momentum",
    original = "momentum",
    func = list(
      pkg = "dials",
      fun = "momentum",
      range = c(0.50, 0.99)
    ),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "tab_resnet",
    eng = "brulee",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(
      pkg = "dials",
      fun = "batch_size",
      range = c(4, 7)
    ),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "tab_resnet",
    eng = "brulee",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_resnet"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tab_resnet",
    eng = "brulee",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "tab_resnet",
    eng = "brulee",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "brulee", fun = "brulee_resnet"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "tab_resnet",
    eng = "brulee",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "tab_resnet",
    eng = "brulee",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = reformat_torch_num,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "numeric"
      )
    )
  )

  parsnip::set_pred(
    model = "tab_resnet",
    eng = "brulee",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "tab_resnet",
    eng = "brulee",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "prob"
      )
    )
  )
}
