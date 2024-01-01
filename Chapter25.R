library("tidyverse")
library("nycflights13")

first_upper <- function(x) {
  str_sub(x, 1, 1) <- str_to_upper(str_sub(x, 1, 1))
  x
}

x <- "hello"
str_sub(x, 1, 1) <- "T"
x
first_upper("hello")
#> [1] "Hello"

prop_average <- function(x) {
  x / sum(x, na.rm = TRUE)
}

round_vector <- function(x) {
  round(x / sum(x, na.rm = TRUE) * 100, 1)
}

rescale02 <- function(x) {
  x = if_else(x == Inf, 1, x)  
  rng <- range(x)
  (x - rng[1]) / (rng[2] - rng[1])
}

z = c(2,0, 1, Inf)
rescale02(z)

convert_to_year <- function(x) {
  x <- if_else(
    make_date(year = year(today()), 
              month = month(x), 
              day = day(x)) < today(), year(today()) - year(x), year(today()) - year(x) - 1)
  x
}
x <- c(ymd("2012-01-01")) 
convert_to_year(x)

compute_veriance <- function(x) {
  x <- (x - mean(x)) ^ 2
  numEvals <- count(x) - 1
  sum(x) / numEvals
}

both_na <- function(x, y) {
  positions <- if_else(is.na(x) & is.na(y), 1, 0)
  sum(positions)
}

x <- c(NA, 1, 2)
y <- c(NA, 2, 3)
both_na(x, y)


find_severe <- function(df, x) {
  df |>
    filter(dep_delay > {{x}} | is.na(arr_time))
}

flights |>
  find_severe(600)

summarize_severe <- function(df) {
  df |>
    find_severe() |>
    count()
}

flights |>
  summarize_severe()

summarize_weather <- function(df, x) {
  df |>
    summarize(
      minimum = min({{x}}, na.rm = TRUE),
      mean = mean({{x}}, na.rm = TRUE),
      maximum = max({{x}}, na.rm = TRUE)
    )
}

summarize_weather(weather, temp)

count_prop <- function(df, var, sort = FALSE) {
  df |>
    count(pick({{ var }}), sort = sort) |>
    mutate(prop = n / sum(n))
}

flights |>
  count_prop(var = c(year, month, day))

scatterplot <- function(df, xAxis, yAxis) {
  label <- rlang::englue("A scatterplot of {{xAxis}} vs {{yAxis}}")
  
  df |>
    ggplot(mapping = aes(x = {{xAxis}}, y = {{yAxis}})) +
    geom_smooth(se = FALSE) + 
    labs(title = label)
}

scatterplot(flights, dep_time, arr_time)
