## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  dpi = 300
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
warning("This article provides an example of using `geom_braid()` with a flipped set of aesthetics `y`, `xmin`, `xmax` instead of the usual `x`, `ymin`, `ymax`. This change is primarily motivated by the fact that we are visualizing judicial ideology scores, from strongly liberal to strongly conservative, and we are accustomed to interpreting ideologies on the left-right political spectrum.")

## ----load-packages-and-data, message = FALSE----------------------------------
library(ggplot2)
library(ggbraid)
library(readr)
library(dplyr)

court <- read_csv(
    "https://mqscores.lsa.umich.edu/media/2020/court.csv",
    col_types = cols_only(term = col_character(), med = col_double())
  ) %>%
  mutate(year = as.integer(substr(term, 1, 4))) %>%
  group_by(year) %>%
  summarize(mqscore = median(med), .groups = "drop") %>%
  arrange(desc(year))

court

## ----court-plot-up-down-------------------------------------------------------
ggplot(court, aes(x = year, y = mqscore)) +
  geom_line() +
  geom_hline(yintercept = 0)

## ----court-plot-left-right-wrong----------------------------------------------
ggplot(court, aes(x = mqscore, y = year)) +
  geom_line() +
  geom_vline(xintercept = 0)

## ----court-plot-left-right-correct, fig.height = 12---------------------------
ggplot(court, aes(x = mqscore, y = year)) +
  geom_path() +
  geom_vline(xintercept = 0)

## ----court-plot-geom-braid, fig.height = 12-----------------------------------
ggplot(court, aes(x = mqscore, y = year)) +
  geom_path() +
  geom_vline(xintercept = 0) +
  geom_braid(
    aes(xmin = mqscore, xmax = 0, fill = mqscore < 0),
    method = "line"
  )

## ----court-plot-geom-braid-improved, fig.height = 12--------------------------
ggplot(court, aes(x = mqscore, y = year)) +
  geom_path() +
  geom_braid(
    aes(xmin = mqscore, xmax = 0, fill = mqscore < 0),
    method = "line",
    alpha = 0.3
  ) + 
  scale_fill_manual(values = c("red", "blue")) +
  guides(fill = "none")

## ----load-justices-data-------------------------------------------------------
justices <- read_csv(
    "https://mqscores.lsa.umich.edu/media/2020/justices.csv",
    col_types = cols_only(
      term = col_integer(),
      justiceName = col_character(),
      post_med = col_double()
    )
  ) %>%
  rename(year = term, name = justiceName, mqscore = post_med) %>%
  arrange(desc(year), name)

justices

## ----justices-plot-not-rescaled, fig.height = 12------------------------------
ggplot(court, aes(x = mqscore, y = year)) +
  geom_path() +
  geom_braid(
    aes(xmin = mqscore, xmax = 0, fill = mqscore < 0),
    method = "line",
    alpha = 0.3
  ) +
  geom_path(aes(group = name), data = justices, size = 0.3, alpha = 0.2) +
  scale_fill_manual(values = c("red", "blue")) +
  guides(fill = "none")

## ----justices-plot-rescaled, fig.height = 12----------------------------------
moderate <- function(x) sign(x) * sqrt(abs(x))

ggplot(court, aes(x = moderate(mqscore), y = year)) +
  geom_path() +
  geom_braid(
    aes(xmin = moderate(mqscore), xmax = 0, fill = moderate(mqscore) < 0),
    method = "line",
    alpha = 0.3
  ) +
  geom_path(aes(group = name), data = justices, size = 0.3, alpha = 0.2) +
  scale_fill_manual(values = c("red", "blue")) +
  guides(fill = "none") +
  labs(x = NULL) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

## ----make-selected-justices, fig.height = 12----------------------------------
selected_justice_names <- c(
  "RBGinsburg" = "Ginsburg",
  "HABlackmun" = "Blackmun",
  "TMarshall" = "Marshall",
  "JHarlan2" = "Harlan II",
  "WHRehnquist" = "Rehnquist",
  "SDOConnor" = "O'Connor",
  "EWarren" = "Warren",
  "CThomas" = "Thomas",
  "WODouglas" = "Douglas"
)

selected_justices <- justices %>%
  filter(name %in% names(selected_justice_names)) %>%
  mutate(name = recode(name, !!! selected_justice_names))

selected_justices

## ----make-selected-justices-text----------------------------------------------
selected_justices_text <- selected_justices %>%
  group_by(name) %>%
  summarize(year = first(year), mqscore = first(mqscore), .groups = "drop") 

selected_justices_text

## ----plot-with-selected-justices, fig.height = 12-----------------------------
ggplot(court, aes(x = moderate(mqscore), y = year)) +
  geom_path() +
  geom_braid(
    aes(xmin = moderate(mqscore), xmax = 0, fill = moderate(mqscore) < 0),
    method = "line",
    alpha = 0.3
  ) +
  geom_path(aes(group = name), data = justices, size = 0.3, alpha = 0.2) +
  geom_path(aes(group = name), data = selected_justices, size = 0.5, alpha = 0.4) +
  geom_text(aes(label = name), data = selected_justices_text, size = 3, alpha = 0.7, vjust = -0.7) +
  scale_fill_manual(values = c("red", "blue")) +
  guides(fill = "none") +
  labs(x = NULL) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

## ----final-plot, fig.height = 12----------------------------------------------
decades <- data.frame(year = seq(1940, 2020, by = 10))

ggplot(court, aes(x = moderate(mqscore), y = year)) +
  geom_path(size = 0.8) +
  geom_braid(
    aes(xmin = moderate(mqscore), xmax = 0, fill = moderate(mqscore) < 0),
    method = "line",
    alpha = 0.3
  ) +
  geom_path(aes(group = name), data = justices, size = 0.3, alpha = 0.2) +
  geom_path(aes(group = name), data = selected_justices, size = 0.5, alpha = 0.4) +
  geom_text(aes(label = name), data = selected_justices_text, size = 4, alpha = 0.7, vjust = -0.7) +
  geom_text(aes(label = year, x = 0), data = decades, size = 4, alpha = 0.4) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.06) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.06) +
  annotate("text", x = 0, y = 2032, label = "Ideological Leaning of the US Supreme Court", fontface = "bold", size = 6.5) +
  annotate("text", x = 0, y = 2029, label = "Adjusted Martin-Quinn scores of every justice serving since 1937", size = 4) +
  annotate("text", x = 3, y = 1938.5, label = "Data from mqscores.lsa.umich.edu\nGraphic from nsgrantham.github.io/ggbraid/articles/court", size = 3, hjust = 1) +
  annotate("text", x = -0.5, y = 2025, label = "← More Liberal", hjust = 1, color = "blue", size = 4.5, alpha = 0.6) +
  annotate("text", x = 0.5, y = 2025, label = "More Conservative →", hjust = 0, color = "red", size = 4.5, alpha = 0.6) +
  annotate("text", x = 0.75, y = 2021.1, label = "Court Median", fontface = "bold", size = 4) +
  scale_x_continuous(limits = c(-3.05, 3.05), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(NA, 2034.5), expand = c(0.01, 0.05)) +
  scale_fill_manual(values = c("red", "blue")) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    plot.margin = margin(0, 0, -3, -3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
  )

