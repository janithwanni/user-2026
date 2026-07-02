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
  geom_point(data = two_dim_data, aes(x = x, y = y, color = class)) +
  geom_point(
    data = data_space_poi,
    aes(x = x, y = y, color = preds),
    shape = 19
  ) +
  geom_point(
    data = data_space_preds,
    aes(x = x, y = y, color = preds),
    alpha = 0.1
  ) +
  labs_call("Wombats of interest") +
  color_pal

lime_plot <- ggplot()
shap_plot <- ggplot()
cf_plot <- ggplot()
anchor_plot <- ggplot()
