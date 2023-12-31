library(tidyverse)
library(repurrrsive)
library(jsonlite)

df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)

df2 |>
  unnest_wider(y, names_sep = c("test"))

gh_tibble <- tibble(gh_repos)
gh_tibble |>
  unnest_longer(gh_repos) |>
  unnest_wider(gh_repos) |>
  unnest_wider(owner, names_sep = "_")

json_col <- parse_json('
  {
    "x": ["a", "x", "z"],
    "y": [10, null, 3]
  }
')
json_row <- parse_json('
  [
    {"x": "a", "y": 10},
    {"x": "x", "y": null},
    {"x": "z", "y": 3}
  ]
')

df_col <- tibble(json = list(json_col)) 
df_row <- tibble(json = json_row)

df_col |>
  unnest_wider(json) |>
  unnest_longer(col = c(x,y))

df_row |>
  unnest_wider(json)
