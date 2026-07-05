logger::log_info("generating kumquats")
kquat_loc <- here::here("data/twod_kquats.rds")
if (CLEAN_RUN) {
  twod_ks <- kumquat::kumquat(
    model_bundle = rf_model_2d_bndl,
    data = two_dim_data,
    pois = data_space_poi,
    predictor_vars = c("x", "y"),
    class_names = c("Accept", "Reject")
  )
  saveRDS(twod_ks, kquat_loc)
} else {
  twod_ks <- readRDS(kquat_loc)
}

twod_ks_tbl <- twod_ks |>
  map(\(x) {
    fi <- x$local_model$importances
    tibble(case = x$point_of_interest$index, feature = names(fi), imps = fi)
  }) |>
  bind_rows()


logger::log_info("generating shaps")
shap_loc <- here::here("data/twod_shaps.rds")

if (CLEAN_RUN) {
  shap_vals <- kernelshap(
    rf_model_2d,
    data_space_poi |> select(x, y),
    bg_X = two_dim_data |> select(x, y) |> slice_sample(n = 500),
    pred_fun = \(x, ...) {
      stats::predict(x, type = "prob", ...)[, 1]
    },
    feature_names = c("x", "y"),
    verbose = FALSE,
    seed = 1835
  )
  saveRDS(shap_vals, here::here(shap_loc))
}
twod_shaps <- readRDS(here::here(shap_loc))

logger::log_info("generating cfacts")
cfact_loc <- here::here("data/twod_cfacts.rds")

if (CLEAN_RUN) {
  predictor_rf <- iml::Predictor$new(
    rf_model_2d,
    data = two_dim_data,
    type = "prob"
  )

  cf_gen <- counterfactuals::NICEClassif$new(
    predictor_rf
  )

  poi_data <- data_space_poi

  cfvals <- map(seq_len(nrow(poi_data)), function(i) {
    obs <- poi_data[i, ]
    new_class <- ifelse(obs$class == "Accept", "Reject", "Accept")
    w_cf <- cf_gen$find_counterfactuals(
      x_interest = obs,
      desired_class = new_class,
      desired_prob = c(0.99, 1)
    )
    # TODO: deal with multiple w_cf in MOCClassif
    return(w_cf$data)
  }) |>
    list_rbind()

  saveRDS(cfvals, here::here(cfact_loc))
}
twod_cfacts <- readRDS(here::here(cfact_loc))


logger::log_info("generating anchors")
anchor_loc <- here::here("data/twod_anchors.rds")

model_func <- carrier::crate(
  function(x) {
    stats::predict(model, x)
  },
  model = rf_model_2d
)

column_names <- c("x", "y")
if (CLEAN_RUN) {
  final_bounds <- make_anchors(
    dataset = two_dim_data,
    cols = column_names,
    instance = data_space_poi,
    model_func = model_func,
    class_col = "class",
    verbose = FALSE,
    seed = 145,
    parallel = FALSE,
    progress = FALSE,
    instance_lbls = data_space_poids
  )
  saveRDS(final_bounds, here::here(anchor_loc))
}
twod_anchors <- readRDS(here::here(anchor_loc))
