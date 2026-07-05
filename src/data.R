deterministic_boundary <- function(data) {
  data |>
    mutate(
      class = factor(case_when(
        x < (-0.5) & y < (-0.3) ~ "Accept",
        x >= -0.5 & x < 0.4 & y < 0.1 + 0.8 * x ~ "Accept",
        x >= 0.4 ~ "Accept",
        .default = "Reject"
      ))
    ) |>
    mutate(class = factor(if_else(y < (-1) + 0.8 * x, "Reject", class)))
}

data(d_multitwo, package = "kumquat")
d_multitwo <- d_multitwo |> deterministic_boundary()
train_indices <- sample(
  seq_len(nrow(d_multitwo)),
  floor(nrow(d_multitwo) * 0.001)
)
two_dim_data <- d_multitwo |>
  dplyr::mutate(index = dplyr::row_number(), class = factor(class))

test_two_dim_data <- d_multitwo |>
  slice(-train_indices) |>
  dplyr::mutate(index = dplyr::row_number(), class = factor(class))


data_space <- expand_grid(x = seq(-1, 1, 0.01), y = seq(-1, 1, 0.01)) |>
  mutate(index = row_number())
data_space <- deterministic_boundary(data_space)

two_dim_pois <- c(3604, 3332, 565, 308, 2778) |> sort()
data_space_poids <- c(27678, 20818, 33218, 28105) |> sort()

two_dim_poi_data <- two_dim_data |>
  dplyr::filter(index %in% two_dim_pois) |>
  dplyr::mutate(label = match(index, two_dim_pois))
