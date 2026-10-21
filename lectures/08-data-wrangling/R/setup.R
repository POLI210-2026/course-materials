# Read-only classroom setup. Source again to restore the input objects.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})
elections <- read_csv("data/elections.csv", show_col_types = FALSE)
ug <- read_csv("data/ug.csv", show_col_types = FALSE)
turk <- read_csv("data/mturk.csv", show_col_types = FALSE)
states <- read_csv("data/states.csv", show_col_types = FALSE)
members <- read_csv("data/house114.csv", show_col_types = FALSE)
options(width = 85, pillar.width = 85)
