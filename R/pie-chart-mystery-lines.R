pacman::p_load("tidyverse")

# The problem is that there are thin white columns in the exported version of the image
pie <-
  tribble(
    ~outcome, ~n, ~total_people, ~pct, ~ypos,
    "Outcome 1", 2, 94, 2.13, .426,
    "Outcome 2", 42, 94, 44.1, 11.1,
    "Outcome 3", 12, 94, 12.8, 49.4,
    "Outcome 4", 38, 94, 40.4, 67.7
  ) %>%
  ggplot(aes(x = "", y = pct, group = outcome)) +
  geom_col(aes(fill = outcome),
    alpha = .8
  ) +
  coord_polar("y", start = 0) +
  scale_y_continuous(labels = NULL) +
  xlab("") +
  ylab("") +
  guides(fill = guide_legend(title = "Outcome")) +
  geom_text(aes(label = paste(round(pct / sum(pct) * 100, 1), "%")),
    position = position_stack(vjust = 0.5)
  ) +
  scale_fill_manual(values = c("grey60", "steelblue", "#333333", "#0000FF")) +
  theme(
    plot.background = element_rect(fill = "#E6E6E6", colour = NA),
    panel.background = element_rect(fill = "#E6E6E6", colour = NA),
    legend.background = element_rect(fill = "#E6E6E6"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.spacing = unit(0, "mm"),
    axis.ticks.length = unit(0, "pt")
  ) +
  guides(colour = FALSE)

pie