library(tidyverse)
library(gt)
library(tidymodels)
library(conflicted)
library(colorspace)
library(patchwork)
library(here)
library(randomForest)
library(DALEX)
library(DALEXtra)
# library(lime)
library(kernelshap)
library(iml)
library(counterfactuals)
library(kultarr) # janithwanni/kultarr
library(tourr)
library(detourr) # janithwanni/detourr
library(furrr)
library(progressr)
plan(sequential)
set.seed(4535)

conflicts_prefer(dplyr::filter)

knitr::opts_chunk$set(
  fig.width = 8,
  fig.height = 8,
  fig.align = "center",
  out.width = "100%",
  code.line.numbers = FALSE,
  fig.retina = 4,
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  dev.args = list(pointsize = 11)
)

options(
  digits = 2,
  width = 60
)

theme_set(
  theme_bw(base_size = 14) +
    theme(
      aspect.ratio = 1,
      plot.background = element_rect(fill = 'transparent', colour = NA),
      plot.title.position = "plot",
      plot.title = element_text(size = 24),
      panel.background = element_rect(fill = 'transparent', colour = NA),
      legend.background = element_rect(fill = 'transparent', colour = NA),
      legend.key = element_rect(fill = 'transparent', colour = NA)
    )
)

class_a_color <- "#1B9E77FF"
class_b_color <- "#D95F02FF"

options(
  ggplot2.discrete.fill = c(class_a_color, class_b_color),
  ggplot2.discrete.color = c(class_a_color, class_b_color)
)

CLEAN_RUN <- as.logical(Sys.getenv("CLEAN_RUN", "FALSE"))
