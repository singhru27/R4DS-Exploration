library(tidyverse)
library(babynames)

x <- r"(He said "That's amazing!")"
str_view(x)

x <- r"(\a\b\c\d\n)"
x <- "This\u00a0is\u00a0tricky"
x
str_view(x)
?str_view

str_c("hi ", NA)
paste0(letters[1:5], letters[1:3])


str_c("The price of ", food, " is ", price)

str_flatten(c("a", "b", "c"))

babynames |>
  mutate(midLetter = str_sub(name, ceiling(str_length(name) / 2), ceiling(str_length(name) / 2)))

babynames |>
  mutate(length = str_length(name)) |>
  group_by(year) |>
  summarize(avg_length = sum(n * length) / sum(n)) |>
  ggplot(mapping = aes(x = year, y = avg_length)) +
  geom_point()

babynames |>
  mutate(first = str_sub(name, 1, 1), last = str_sub(name, -1, -1)) |>
  group_by(year, first) |>
  mutate(firstFrequency = n()) |>
  ungroup() |>
  group_by(year, last) |>
  mutate(lastFrequency = n()) |>
  group_by(year) |>
  filter(firstFrequency == max(firstFrequency)) |>
  distinct(year, first)

a <- "a\\r"
str_length(a)  
