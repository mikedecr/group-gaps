# Knitr chunks:
# - eval but don't show
# - hide code and messages
# - cache everything
knitr::opts_chunk$set(
  eval = TRUE, echo = FALSE, include = FALSE,
  warning = FALSE, message = FALSE,
  cache = TRUE, collapse = TRUE,
  fig.path = here("paper", "chunk-figs/"),
  cache.path = here("paper", "chunk-cache/"),
  # fig.retina = 2, dpi = 100,
  dev = "cairo_pdf", fig.align = "center"
)
