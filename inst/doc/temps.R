## ----set-chunk-opts, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%"
)

knitr::knit_hooks$set(
  warning = function(x, options) {
    paste(
      '\n\n<div class="alert alert-info">',
      gsub('##', '\n', gsub('Warning: ', '', x)),
      '</div>',
      sep = '\n'
    )
  }
)

## ----about-the-article, echo = FALSE, comment = ""----------------------------
warning("This article presents the \"Unbraided Ribbon Problem\" in which `geom_ribbon()` incorrectly fills the area between two alternating lines with two different colors. To fix the problem, we use `geom_braid()` from ggbraid with `method = 'line'`.")

## ----temps, message=FALSE-----------------------------------------------------
library(ggplot2)
library(ggbraid)
library(dplyr)
library(tidyr)

data(temps)

temps

## ----temps-plot---------------------------------------------------------------
ggplot(temps) +
	geom_line(aes(x = date, y = avg, linetype = city))

## ----temps-plot-polished------------------------------------------------------
p <- ggplot() +
  geom_line(aes(x = date, y = avg, linetype = city), data = temps) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(
    breaks = seq(20, 90, by = 10),
    labels = function(x, ...) format(paste(x, "°F"), ...),
    limits = c(18, 90)
  ) +
  guides(fill = "none") +
  labs(
    title = "Average Daily Temperatures in 2021",
    linetype = NULL,
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
  	plot.title = element_text(size = 15),
    plot.title.position = "plot",
  	legend.position = c(0.75, 1.06),
  	legend.direction = "horizontal",
  	legend.key.size = unit(2, "line"),
  	legend.text = element_text(size = 12),
    panel.grid.major.x = element_line(size = 0.4),
    panel.grid.major.y = element_line(size = 0.4),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

p

## ----temps-wide---------------------------------------------------------------
temps_wide <- temps %>%
  pivot_wider(names_from = city, values_from = avg) %>%
  rename(ny = `New York`, sf = `San Francisco`)

temps_wide

## ----geom-ribbon-without-fill-------------------------------------------------
p +
  geom_ribbon(
    aes(x = date, ymin = ny, ymax = sf),
    data = temps_wide,
    alpha = 0.3
  )

## ----geom-ribbon-with-fill----------------------------------------------------
p +
  geom_ribbon(
    aes(x = date, ymin = ny, ymax = sf, fill = sf > ny),
    data = temps_wide,
    alpha = 0.7
  )

## ----geom-braid-with-fill-----------------------------------------------------
p +
  geom_braid(
    aes(x = date, ymin = ny, ymax = sf, fill = sf > ny),
    data = temps_wide,
    alpha = 0.7
  )

## ----geom-ribbon-stat-braid-with-fill-----------------------------------------
p +
  geom_ribbon(
    aes(x = date, ymin = ny, ymax = sf, fill = sf > ny),
    data = temps_wide,
    stat = "braid",
    method = "line",
    alpha = 0.7
  )

## ----geom-braid-with-fill-and-labels------------------------------------------
hues <- scales::hue_pal()(2)  # ggplot2 default color palette

p +
  geom_braid(
    aes(x = date, ymin = ny, ymax = sf, fill = sf > ny),
    data = temps_wide,
    method = "line",
    alpha = 0.7
  ) +
  annotate("text", x = as.Date("2021-09-10"), y = 84, size = 4, label = "NY hotter than SF", hjust = 0, color = hues[1]) +
  annotate("text", x = as.Date("2021-02-20"), y = 23, size = 4, label = "NY colder than SF", hjust = 0, color = hues[2])

