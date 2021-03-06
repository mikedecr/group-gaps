# packages
library("knitr")
library("here")
library("magrittr")
library("tidyverse")
library("ggplot2")
library("scales")
library("labelled")
library("broom")
library("latex2exp")


# Knitr chunks:
# - eval but don't show
# - hide code and messages
# - cache everything
knitr::opts_chunk$set(
  eval = TRUE, echo = FALSE, include = FALSE, 
  warning = FALSE, message = FALSE,
  cache = TRUE, collapse = TRUE,
  fig.path = "figs/",
  # cache path = ...,
  # fig.retina = 2, dpi = 100,
  dev = "cairo_pdf", fig.align = "center"
)

# graphics theme
theme_set(
  ggthemes::theme_base(base_family = "Source Sans Pro", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)

