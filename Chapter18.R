library(tidyverse)
library(nycflights13)

flights |>
  distinct(tailnum) |>
  anti_join(planes) |>
  left_join(flights) |>
  select(carrier) |>
  count(carrier)
