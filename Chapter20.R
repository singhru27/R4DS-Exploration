library(readxl)
library(tidyverse)
library(writexl)

surveys <- read_excel("data/survey.xlsx",
                      col_names = c("survey_id", "n_pets"),
                      skip = 1,
                      col_types = c("text", "text"),
                      na = c("", "N/A")
                      )

surveys <- surveys |>
  mutate(
    n_pets = if_else(n_pets == "two", "2.0", n_pets)
  ) |>
  mutate (
    n_pets = parse_number(n_pets)
  )

surveys

roster <- read_excel("data/roster.xlsx") |>
  fill(subgroup, group)
roster

sales <- read_excel("data/sales.xlsx",
                    range = "A5:B13",
                    col_names = c("id", "n")
                    )
sales |>
  mutate(brand = if_else(str_detect(id, "Brand"), id, NA)) |>
  fill(brand) |>
  filter(!str_detect(id, "Brand"))



