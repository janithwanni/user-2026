labs_call <- function(
  x = "Cute",
  y = "Chonky",
  color = "Eligible",
  title = "",
  caption = "",
  subtitle = ""
) {
  labs(
    x = x,
    y = y,
    color = color,
    title = title,
    caption = caption,
    subtitle = subtitle
  )
}

color_pal <- list(
  scale_color_manual(values = c(class_a_color, class_b_color)),
  scale_fill_manual(values = c(class_a_color, class_b_color))
)


train_data_fig <- ggplot() +
  geom_point(data = two_dim_data, aes(x = x, y = y, color = class)) +
  labs_call(title = "Training data") +
  color_pal

model_preds_fig <- ggplot() +
  geom_point(data = data_space_preds, aes(x = x, y = y, color = preds)) +
  labs_call(title = "Model boundary") +
  color_pal

poi_data_fig <- ggplot() +
  geom_point(
    data = two_dim_data,
    aes(x = x, y = y, color = class),
    alpha = 0.3
  ) +
  geom_point(
    data = data_space_poi,
    aes(x = x, y = y, color = preds),
    size = 4,
    shape = 18
  ) +
  geom_point(
    data = data_space_preds,
    aes(x = x, y = y, color = preds),
    alpha = 0.1
  ) +
  geom_text(
    data = data_space_poi,
    aes(x = x, y = y, label = wombat_name),
    nudge_x = -0.05,
    nudge_y = -0.05,
    fontface = "bold",
    color = "black"
  ) +
  labs_call(title = "Wombats of interest") +
  color_pal

lime_plot <- ggplot()

shap_plot <- poi_data_fig +
  geom_segment(
    data = tibble(
      x = data_space_poi$x,
      x_end = x + two_dim_shap_poi$x,
      y = data_space_poi$y,
      y_end = y
    ),
    aes(x = x, y = y, xend = x_end, yend = y_end),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.1, "cm")),
    lineend = "round",
    linejoin = "bevel",
    show.legend = FALSE
  ) +
  geom_segment(
    data = tibble(
      x = data_space_poi$x,
      x_end = x,
      y = data_space_poi$y,
      y_end = y + two_dim_shap_poi$y
    ),
    aes(x = x, y = y, xend = x_end, yend = y_end),
    inherit.aes = FALSE,
    arrow = arrow(length = unit(0.1, "cm")),
    lineend = "round",
    linejoin = "bevel",
    show.legend = FALSE
  ) +
  color_pal


smaller_cf_plots <- NULL
cf_plot <- ggplot()
anchor_plot <- ggplot()
