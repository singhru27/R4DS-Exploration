library(tidyverse)
library(nycflights13)


newFlights <- 
  flights |>
  select(dep_time, sched_dep_time, dep_delay)

newFlights |> 
  filter(is.na(dep_time)) |>
  count(dep_time, dep_delay)

newFlights |> 
  filter(is.na(dep_delay)) |>
  count(dep_time, dep_delay)

newFlights |> 
  filter(is.na(sched_dep_time)) |>
  count(sched_dep_time)

flights |>
  filter (is.na(arr_delay) & !is.na(dep_delay))

flights |>
  filter(is.na(arr_delay) & !is.na(arr_time) & !is.na(sched_arr_time))

cancelled <- flights |>
  filter(is.na(dep_time))

flights |>
  mutate(dayYear =month * 12 + day) |>
  group_by(dayYear) |>
  summarise(cancellations = sum(is.na(dep_time)) / n(), avgDelay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot(mapping = aes(x = cancellations, y = avgDelay)) + 
  scale_x_continuous(labels = label_percent()) + 
  geom_point()

x <- c(0:20)
if_else(x %% 2 == 0, "EVEN", "ODD")

x <- c("Monday", "Saturday", "Wednesday")
weekends <- c("Saturday", "Sunday")
if_else (x %in% weekends, "Weekend", "Weekday")

if_else(x < 0, -x, x)