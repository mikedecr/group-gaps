# FONT_FAMILY <- "Source Sans Pro"
box::use(ggplot2[...])
box::use(ggthemes[theme_base])
box::use(purrr[partial])


# ----- graphics theme ----------

theme_gaps <-
    theme_base(base_size = 14) +
    # theme_base(base_family = FONT_FAMILY, base_size = 14) +
    theme(
        plot.background = element_blank(),
        axis.ticks = element_line(lineend = "square"),
        axis.ticks.length = grid::unit(0.25, "lines")
    )

theme_set(theme_gaps)

DCOLOR <- "#0072B2"
RCOLOR <- "#D55E00"

party_colors = c("Dem" = DCOLOR, "Rep" = RCOLOR)
scale_color_party <- partial(scale_color_manual, values = party_colors)
scale_fill_party <- partial(scale_fill_manual, values = party_colors)

MCOLOR <- "#009E73"
WCOLOR <- "#E69F00"

gender_colors <- c("M" = MCOLOR, "W" = WCOLOR)
scale_color_gender <- partial(scale_color_manual, values = gender_colors)
scale_fill_gender <- partial(scale_fill_manual, values = gender_colors)


