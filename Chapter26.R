library(tidyverse)
library(palmerpenguins)

penguins |>
  summarize(
    across(everything(), n_distinct)
  )

mtcars |>
  summarize (
    across(everything(), mean)
  )

diamonds |>
  group_by(cut, clarity, color) |>
  summarize (
    across(where(is.numeric), 
           list(mean = mean)
           ),
    n = n()
  )

diamonds |>
  select(price) |>
  summarize(
    across(
      everything(),
      list(mean, median)
    )
  )
