guess_ks_part <- twod_ks_tbl |>
  group_by(case) |>
  arrange(feature) |>
  ungroup() |>
  select(case, guess = feature, lime = imps) |>
  mutate(case = as.integer(case))

guess_shap_part <- twod_shaps$S |>
  as_tibble() |>
  mutate(case = data_space_poids) |>
  pivot_longer(cols = x:y) |>
  group_by(case) |>
  arrange(name) |>
  select(case, guess = name, SHAP = value) |>
  mutate(case = as.integer(case))

guess_cf_part <- data_space_poi |>
  cbind(twod_cfacts |> rename(xc = x, yc = y) |> select(xc, yc)) |>
  mutate(dist1 = abs(xc - x), dist2 = abs(yc - y)) |>
  pivot_longer(c(dist1, dist2)) |>
  group_by(index) |>
  arrange(name) |>
  select(case = index, guess = name, Counterfactuals = value) |>
  mutate(
    case = as.integer(case),
    guess = case_when(
      guess == "dist1" ~ "x",
      guess == "dist2" ~ "y"
    )
  )

twod_anchor_poi_data <- twod_anchors |>
  pluck("final_anchor") |>
  mutate(
    x = case_when(
      is.na(x) & bound == "lower" ~ min(data_space_preds$x),
      is.na(x) & bound == "upper" ~ max(data_space_preds$x),
      TRUE ~ x
    ),
    y = case_when(
      is.na(y) & bound == "lower" ~ min(data_space_preds$y),
      is.na(y) & bound == "upper" ~ max(data_space_preds$y),
      TRUE ~ y
    )
  )

guess_anchor_part <- twod_anchor_poi_data |>
  group_by(id) |>
  summarise(across(x:y, \(x) abs(x[1] - x[2]))) |>
  ungroup() |>
  pivot_longer(c(x, y)) |>
  group_by(id) |>
  arrange(name) |>
  select(case = id, guess = name, Anchors = value) |>
  mutate(case = as.integer(case))

guess_table <-
  left_join(
    guess_ks_part,
    guess_shap_part
  ) |>
  left_join(guess_cf_part) |>
  left_join(
    guess_anchor_part
  ) |>
  mutate(across(SHAP:Anchors, abs)) |>
  # mutate(Anchors = 2 - Anchors) |>
  arrange(case, guess) |>
  rename(feature = guess)

disagree_table <- guess_table |>
  pivot_longer(lime:Anchors) |>
  group_by(case, name) |>
  filter(abs(value) == max(abs(value))) |>
  select(case, name, feature) |>
  pivot_wider(
    id_cols = case,
    names_from = name,
    values_from = feature,
    values_fn = \(x) {
      paste(x, collapse = ',')
    }
  )

## ---- SHAP
two_dim_shap_poi <- twod_shaps$S |> as_tibble()


## --- CF
twod_cfact_poi <- data_space_poi |>
  cbind(twod_cfacts |> rename(xc = x, yc = y) |> select(xc, yc))


disagree_table_show <- disagree_table |>
  ungroup() |>
  mutate(across(lime:Anchors, function(x) {
    x <- gsub("x", "cuteness", x)
    x <- gsub("y", "chonky", x)
    return(x)
  })) |>
  right_join(data_space_poi |> select(case = index, wombat_name)) |>
  select(Wombat = wombat_name, Lime = lime, SHAP, Counterfactuals, Anchors)
