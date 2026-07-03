model_location <- here::here("models/two_dim_rfmodel.rds")

if (CLEAN_RUN) {
  rf_model_2d_raw <- randomForest(
    class ~ x + y,
    data = two_dim_data,
    ntree = 10,
    mtry = 2
  )
  rf_model_2d_bndl <- bundle::bundle(rf_model_2d_raw)
  saveRDS(rf_model_2d_bndl, model_location)
}
rf_model_2d_bndl <- readRDS(here::here(model_location))

rf_model_2d <- rf_model_2d_bndl |>
  bundle::unbundle()


rf_model_2d <- rf_model_2d_bndl |>
  bundle::unbundle()


data_space_preds <- data_space |>
  mutate(
    preds = predict(
      rf_model_2d,
      data_space
    )
  )

wombat_names <- c("elle", "minibus", "ringo", "kato")

data_space_poi <- data_space_preds |>
  dplyr::filter(index %in% data_space_poids) |>
  dplyr::mutate(label = match(index, data_space_poids)) |>
  dplyr::mutate(wombat_name = wombat_names[label])

two_dim_data <- two_dim_data |>
  mutate(preds = predict(rf_model_2d, two_dim_data))
