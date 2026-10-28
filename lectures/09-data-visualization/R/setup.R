library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_minimal(base_size = 15))
elections <- read_csv("data/elections.csv", show_col_types = FALSE) |>
  mutate(incumbent = factor(inc3, levels = c(-1, 0, 1),
    labels = c("Republican", "Open seat", "Democratic")))
e2014 <- elections |> filter(year == 2014)
recent <- elections |> filter(year %in% c(2004, 2006, 2008, 2010, 2014))
pres <- elections |> filter(!is.na(dpres))
linked <- read_csv("data/elections-nominate.csv", show_col_types = FALSE)
matched <- linked |> filter(match_status == "Matched", !is.na(dpres))
house <- read_csv("data/house114.csv", show_col_types = FALSE) |>
  filter(party_code %in% c(100, 200), !is.na(nominate_dim1)) |>
  mutate(party = factor(party_code, levels = c(100, 200), labels = c("Democratic", "Republican")))
inc_colors <- c(Republican = "#b35806", "Open seat" = "#777777", Democratic = "#2166ac")
party_colors <- c(Democratic = "#2166ac", Republican = "#b35806")
