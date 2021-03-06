## ----set-chunk-opts, include = FALSE------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.height = 5
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
warning("This article demonstrates how to use `geom_braid()` with `method = 'step'` to fill the area between two alternating steps drawn with `geom_step()`.")

## ----hoops, message=FALSE-----------------------------------------------------
library(ggplot2)
library(ggbraid)
library(ggtext)  # element_markdown()
library(dplyr)
library(tidyr)
library(glue)    # glue()
library(hms)     # as_hms()

data(hoops)

hoops

## ----hoops-score, message=FALSE-----------------------------------------------
hoops_score <- hoops %>%
  group_by(team) %>%
  mutate(score = cumsum(points)) %>%
  ungroup()

hoops_score

## ----hoops-score-plot---------------------------------------------------------
ggplot(hoops_score) +
  geom_step(aes(x = time, y = score, color = team))

## ----hoops-wide---------------------------------------------------------------
hoops_wide <- hoops %>%
  group_by(time, team) %>%
  summarize(points = sum(points), .groups = "drop") %>%
  pivot_wider(names_from = team, values_from = points) %>%
  rename_with(tolower) %>%
  replace_na(list(cle = 0, gsw = 0)) %>%
  mutate(
    cle = cumsum(cle),
    gsw = cumsum(gsw),
    diff = gsw - cle
  )

hoops_wide

## ----hoops-wide-plot----------------------------------------------------------
ggplot(hoops_wide) +
  geom_step(aes(x = time, y = diff)) +
  geom_hline(yintercept = 0)

## ----hoops-wide-plot-polished-------------------------------------------------
gsw_color <- "#006BB6"
cle_color <- "#860038"

p <- ggplot(hoops_wide) +
  geom_step(aes(x = time, y = diff)) +
  geom_hline(yintercept = 0) +
  scale_x_time(
    breaks = as_hms(c("00:00:00", "00:12:00", "00:24:00", "00:36:00", "00:48:00", "00:53:00")),
    labels = c("00:00", "12:00", "24:00", "36:00", "48:00", "53:00"),
    expand = c(0.015, -0.015)
  ) +
  scale_y_continuous(
    breaks = seq(-12, 12, by = 1),
    labels = c(
      glue("<span style='color:{cle_color}'>+{12:1}</span>"),
      "<span style='color:black'>0</span>",
      glue("<span style='color:{gsw_color}'>+{1:12}</span>")
    )
  ) +
  scale_fill_manual(values = c(cle_color, gsw_color)) +
  guides(fill = "none") +
  annotate("text", x = as_hms("00:06:00"), y = 11, label = "Q1", size = 6, color = "lightgrey") +
  annotate("text", x = as_hms("00:18:00"), y = 11, label = "Q2", size = 6, color = "lightgrey") +
  annotate("text", x = as_hms("00:30:00"), y = 11, label = "Q3", size = 6, color = "lightgrey") +
  annotate("text", x = as_hms("00:42:00"), y = 11, label = "Q4", size = 6, color = "lightgrey") +
  annotate("text", x = as_hms("00:50:30"), y = 11, label = "OT1", size = 6, color = "lightgrey") +
  labs(
    title = "2018 NBA Finals Game 1",
    subtitle = glue("<span style='color:{gsw_color};'>Golden State Warriors</span> vs. <span style='color:{cle_color}'>Cleveland Cavaliers</span>"),
    y = NULL,
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_markdown(size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 14, margin = margin(b = 1, unit = "line")),
    axis.text.y = element_markdown(),
    axis.text.x = element_text(color = "grey60"),
    panel.grid.major.x = element_line(size = 0.5),
    panel.grid.major.y = element_line(size = 0.3),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

p

## ----geom-ribbon-hoops--------------------------------------------------------
p +
  geom_ribbon(
    aes(x = time, ymin = diff, ymax = 0),
    alpha = 0.2
  )

## ----geom-braid-hoops-without-fill--------------------------------------------
p +
  geom_braid(
    aes(x = time, ymin = diff, ymax = 0),
    method = "step",
    alpha = 0.2
  )

## ----geom-braid-hoops-with-fill-----------------------------------------------
p +
  geom_braid(
    aes(x = time, ymin = diff, ymax = 0, fill = diff > 0),
    method = "step",
    alpha = 0.7
  )

