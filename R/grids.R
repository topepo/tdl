#' @export
resnet_grid_space_filling <- function(
  wflow,
  num_layers = 3,
  size = 10,
  param_info = NULL,
  range = c(2, 50),
  collapse = TRUE
) {
  if (is.null(param_info)) {
    param_info <- wflow |> extract_parameter_set_dials()
  }
  tune_hidden <- any(param_info$id == "hidden_units")
  tune_batch <- any(param_info$id == "batch_norm_units")

  p <- num_layers * (tune_batch + 1)

  param_expanded <- vector(mode = "list", length = p)
  names(param_expanded)[1:num_layers] <- recipes::names0(num_layers, ".hidden_")

  if (tune_batch) {
    names(param_expanded)[(num_layers + 1):p] <- recipes::names0(
      num_layers,
      ".batch_"
    )
  }

  for (i in 1:p) {
    param_expanded[[i]] <- hidden_units(range)
  }

  param_expanded <- parameters(param_expanded)

  param_expanded <- dplyr::bind_rows(param_expanded, param_info)
  if (tune_hidden) {
    param_expanded <- dplyr::filter(param_expanded, id != "hidden_units")
  }
  if (tune_batch) {
    param_expanded <- dplyr::filter(param_expanded, id != "batch_norm_units")
  }

  class(param_expanded) <- class(param_info)

  grd <- grid_space_filling(param_expanded, size = size)
  if (collapse) {
    hidden_grid <-
      grd |>
      dplyr::select(dplyr::matches("^\\.hidden_")) |>
      apply(1, function(x) unname(x), simplify = FALSE)
    batch_grid <- grd |>
      dplyr::select(dplyr::matches("^\\.batch_")) |>
      apply(1, function(x) unname(x), simplify = FALSE)

    unit_grid <- dplyr::tibble(
      hidden_units = hidden_grid,
      batch_norm_units = batch_grid
    )
    grd <-
      bind_cols(
        unit_grid,
        grd |>
          dplyr::select(
            -dplyr::matches("^\\.hidden"),
            -dplyr::matches("^\\.batch")
          )
      )
  }
  grd
}
