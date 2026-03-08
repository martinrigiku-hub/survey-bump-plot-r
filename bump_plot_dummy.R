library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggbump)

set.seed(1234)

# -----------------------------
# 1) Create dummy data
# -----------------------------
n_groups <- 9
n_items <- 24
n_per_group <- 180

group_labels <- c(
  "1" = "Group A",
  "2" = "Group B",
  "3" = "Group C",
  "4" = "Group D",
  "5" = "Group E",
  "6" = "Group F",
  "7" = "Group G",
  "8" = "Group H",
  "9" = "Group I"
)

item_names <- paste0("ITEM_", 1:n_items)

group_effects <- c(-0.30, -0.15, -0.05, 0.00, 0.08, 0.15, 0.22, -0.10, 0.12)
item_effects <- seq(-0.35, 0.35, length.out = n_items) + rnorm(n_items, 0, 0.08)

dummy_df <- expand.grid(
  id = 1:n_per_group,
  Group = 1:n_groups
) %>%
  arrange(Group, id)

for (i in seq_len(n_items)) {
  latent <- 3.2 + group_effects[dummy_df$Group] + item_effects[i] + rnorm(nrow(dummy_df), 0, 0.85)
  dummy_df[[item_names[i]]] <- pmin(pmax(round(latent), 1), 5)
}

# optional missingness
for (i in item_names) {
  miss_id <- sample(seq_len(nrow(dummy_df)), size = round(0.03 * nrow(dummy_df)))
  dummy_df[[i]][miss_id] <- NA
}

# -----------------------------
# 2) Visual settings
# -----------------------------
group_colors <- c(
  "Group A" = "#E69F00",
  "Group B" = "#56B4E9",
  "Group C" = "#009E73",
  "Group D" = "#F5C710",
  "Group E" = "#0072B2",
  "Group F" = "#D55E00",
  "Group G" = "#CC79A7",
  "Group H" = "#000000",
  "Group I" = "#999999"
)

group_linetypes <- c(
  "Group A" = "solid",
  "Group B" = "dashed",
  "Group C" = "dotted",
  "Group D" = "solid",
  "Group E" = "dashed",
  "Group F" = "dotted",
  "Group G" = "solid",
  "Group H" = "dashed",
  "Group I" = "dotted"
)

# -----------------------------
# 3) Prepare plot data
# -----------------------------
plot_df <- dummy_df %>%
  select(Group, all_of(item_names)) %>%
  pivot_longer(
    cols = all_of(item_names),
    names_to = "item",
    values_to = "value"
  ) %>%
  filter(!is.na(value), !is.na(Group)) %>%
  mutate(
    group = recode(as.character(Group), !!!group_labels),
    item_num = as.numeric(str_extract(item, "\\d+$")),
    item_label = factor(item_num, levels = 1:n_items, labels = 1:n_items)
  ) %>%
  group_by(item_num, item_label, group) %>%
  summarise(
    score = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(item_num, item_label) %>%
  mutate(
    rank = rank(-score, ties.method = "min")
  ) %>%
  ungroup() %>%
  mutate(
    group = factor(group, levels = names(group_colors))
  ) %>%
  arrange(item_num, rank)

right_labels <- plot_df %>%
  filter(item_num == max(item_num))

# -----------------------------
# 4) Create and save plot
# -----------------------------
ggplot(
  plot_df,
  aes(x = item_label, y = rank, color = group, linetype = group, group = group)
) +
  geom_bump(linewidth = 1, smooth = 8, alpha = 0.9) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(
    data = right_labels,
    aes(label = group),
    hjust = -0.4,
    size = 6,
    show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:n_groups) +
  scale_color_manual(values = group_colors) +
  scale_linetype_manual(values = group_linetypes) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Item",
    y = "Rank by mean score",
    title = "Ranking of groups across 24 survey items",
    subtitle = "Groups are ranked by mean item score from 1 (highest) to 9 (lowest). \ncrossing lines show rank shifts across items."
  ) +
  theme_minimal(base_size = 18, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 16, colour = "grey25"),
    plot.margin = margin(20, 120, 20, 25)
  )

ggsave("bump_plot_dummy.png", width = 12, height = 8, dpi = 300)
