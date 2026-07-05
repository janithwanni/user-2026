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

lime_plot <- ggplot(mapping = aes(x = x, y = y, color = preds)) +
  geom_point(data = two_dim_data, alpha = 0.1) +
  geom_point(
    data = twod_ks |>
      map_dfr(
        ~ .x$perturbations |> mutate(glm_preds = .x$local_model$glm_predictions)
      ),
    mapping = aes(x = x, y = y, color = glm_preds),
    alpha = 0.4
  ) +
  geom_point(
    data = data_space_poi,
    color = "black",
    size = 2.5,
    shape = 18,
    alpha = 1
  ) +
  geom_text(
    data = data_space_poi,
    aes(label = label),
    nudge_x = -0.05,
    nudge_y = -0.05,
    fontface = "bold",
    color = "black"
  ) +
  color_pal

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


smaller_cf_plots <- map(unique(twod_cfact_poi$label), \(i) {
  cfact_row <- twod_cfact_poi |> filter(label == i)
  dist <- max(abs(cfact_row$x - cfact_row$xc), abs(cfact_row$y - cfact_row$yc))
  xmean <- mean(cfact_row$x, cfact_row$xc)
  ymean <- mean(cfact_row$y, cfact_row$yc)
  xlim <- c(xmean - dist - 1e-2, xmean + dist + 1e-2)
  ylim <- c(ymean - dist - 1e-2, ymean + dist + 1e-2)
  ggplot() +
    geom_point(
      data = data_space_preds,
      mapping = aes(x = x, y = y, color = preds),
      alpha = 0.5,
      size = 1,
      show.legend = FALSE
    ) +
    geom_point(
      data = data_space_poi |> filter(label == i),
      mapping = aes(x = x, y = y, color = class),
      size = 2.5,
      alpha = 1,
      shape = 18,
      show.legend = FALSE
    ) +
    geom_text(
      data = data_space_poi |> filter(label == i),
      aes(x = x, y = y, label = label),
      nudge_x = -1e-2,
      nudge_y = -1e-2,
      fontface = "bold",
      color = "black"
    ) +
    geom_segment(
      data = cfact_row,
      mapping = aes(x = x, y = y, xend = xc, yend = yc),
      inherit.aes = FALSE
    ) +
    geom_point(
      data = twod_cfact_poi |>
        mutate(
          class = ifelse(data_space_poi$class == "Accept", "Reject", "Accept")
        ) |>
        filter(label == i),
      mapping = aes(x = xc, y = yc, color = class),
      inherit.aes = FALSE,
      show.legend = FALSE,
      shape = 9,
      size = 2.5,
      alpha = 1
    ) +
    coord_fixed(xlim = xlim, ylim = ylim) +
    color_pal +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
})

main_cf_plot <- poi_data_fig +
  geom_segment(
    data = twod_cfact_poi,
    aes(x = x, y = y, xend = xc, yend = yc),
    inherit.aes = FALSE
  ) +
  geom_point(
    data = twod_cfact_poi |>
      mutate(
        class = ifelse(data_space_poi$class == "Accept", "Reject", "Accept")
      ),
    aes(x = xc, y = yc),
    inherit.aes = FALSE,
    shape = 9,
    size = 2.5,
    alpha = 1
  ) +
  color_pal

cf_plot <- main_cf_plot + patchwork::wrap_plots(smaller_cf_plots)

anchor_plot <- poi_data_fig +
  rule_rect_layers(
    twod_anchors$final_anchor,
    fill = "black",
    alpha = 0.7
  ) +
  rule_rect_layers(
    twod_anchors$perturb_bounds,
    fill = "gray",
    alpha = 0.3,
    linewidth = 1.25,
    color = "black"
  ) +
  geom_point(
    data = data_space_poi,
    aes(x = x, y = y),
    color = "black",
    size = 2.5,
    alpha = 1,
    shape = 18
  ) +
  color_pal
