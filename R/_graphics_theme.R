library("ggplot2")


FONT_FAMILY <- "Source Sans Pro"

update_geom_defaults("text", list(family = FONT_FAMILY))

theme_gaps <-
    ggthemes::theme_base(base_family = FONT_FAMILY, base_size = 14) +
    theme(
        plot.background = element_blank(),
        axis.ticks = element_line(lineend = "square"),
        axis.ticks.length = unit(0.25, "lines")
    )

theme_set(theme_gaps)
