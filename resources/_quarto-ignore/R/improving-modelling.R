library(googlesheets4)
library(dplyr)
library(tidyr)
library(ggplot2)

# get responses & simplified question labels
data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1oCS018Tts-vZeR358iPor5hkyvJOAFhP_BjUQT6BsjE/edit?resourcekey#gid=458285708")
cols <- read_sheet("https://docs.google.com/spreadsheets/d/1oCS018Tts-vZeR358iPor5hkyvJOAFhP_BjUQT6BsjE/edit?resourcekey#gid=458285708", sheet = "colnames")

# sort out messy questions
data <- data_raw |>
  mutate(participant = row_number()) |>
  select(-Timestamp) |>
  pivot_longer(cols = !participant, names_to = "question") |>
  left_join(cols, by = c("question" = "colname_raw"))

# Multichoice questions ----------------------
multichoice <- data |>
  filter(!is.na(theme) & past_future %in% c("Past", "Future")) |>
  mutate(recommendation = forcats::fct_reorder(recommendation, order))

responses <- c(
  "Considerably improved", "Improved",
  "No change", "Don't know",
  "Worsened", "Considerably worsened"
)

# count responses
summary <- multichoice |>
  mutate(response = ordered(value,
                         levels = responses,
                         labels = responses),
         past_next = ordered(past_future,
                             levels = c("Past", "Future"))) |>
  group_by(theme, recommendation, past_future, response) |>
  summarise(count = n(),
            pct = count / 14)

# set plot colours
value_colours <- c(
  "Considerably improved" = "#1a9850",
  "Improved" = "#91cf60",
  "No change" = "#fee08b",
  "Don't know" = "#e0e0e0",
  "Worsened" = "#fc8d59",
  "Considerably worsened" = "#d73027"
)

# plot
plot_multichoice <- summary |>
  ggplot(aes(y = past_future, x = count,
             fill = response)) +
  geom_col() +
  scale_fill_manual(values = value_colours) +
  labs(x = NULL, y = NULL, fill = NULL,
       subtitle = "Conditions for outbreak response modelling") +
  facet_wrap(~recommendation, ncol=1, drop = FALSE, ) +
  theme_classic() +
  theme(legend.position = "bottom",
        strip.background = element_blank())


# Comments ----------------------------------------------------------------
comments <- filter(data, past_future == "action") |>
  drop_na(value) |>
  select(participant, theme, actions = value) |>
  group_by(theme) |>
  arrange()
knitr::kable(comments)

