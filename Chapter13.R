library(tidyverse)
library(nycflights13)

flights |> count(dest, sort = TRUE)

flights |>
  group_by(dest) |>
  summarize(n = n ()) |>
  arrange(desc(n))

flights
flights |> count(tailnum, wt = distance)

flights |>
  group_by(tailnum) |>
  summarize(n = sum(distance))

x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))

flights |> 
  filter(month == 1, day == 1) |> 
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()

flights

flights |> 
  filter(month == 1, day == 1) |> 
  mutate(dep_time = dep_time / 60) |>
  mutate(sched_dep_time = sched_dep_time / 60) |>
  ggplot(aes(x = sched_dep_time, y = dep_delay)) +
  geom_point()

flights |>
  mutate(dep_time = round(dep_time / 5) * 5, arr_time = round(arr_time / 5) * 5)

flights |>
  mutate(delayRank = min_rank(dep_delay)) |>
  relocate(delayRank) |>
  arrange(delayRank)

flights |>
  mutate(delayRank = min_rank(dep_delay)) |>
  relocate(delayRank) |>
  filter(delayRank <= 10) |>
  top_n(n = 10, wt = delayRank)

flights |>
  group_by(tailnum) |>
  summarize(propOnTime = mean(is.na(dep_time))) |>
  arrange(desc(propOnTime))

flights |>
  filter(tailnum == "D942DN")

flights |>
  filter(!is.na(tailnum)) |>
  mutate(onTime = !is.na(arr_time) & arr_delay <= 0) |>
  group_by(tailnum) |>
  summarize(numFlights = n(), meanOnTime = mean(onTime)) |>
  filter(numFlights >= 20) |>
  filter(min_rank(meanOnTime) <= 10)

flights |>
  mutate(onTime = !is.na(arr_time) & arr_delay <= 0) |>
  relocate(onTime) |>
  mutate(dep_time = round( ((dep_time %/% 100) * 60 + dep_time %%100) / 24 / 60 * 24) ) |>
  arrange(desc(dep_time)) |>
  group_by(dep_time) |>
  summarize(meanOnTime = mean(onTime, na.rm = TRUE), numFlights = n()) |>
  ggplot() +
  geom_point(mapping = aes(x = dep_time, y = meanOnTime))

flights |>
  group_by(dest) |>
  summarize(totalDelay = sum(dep_delay))

flights |> 
  mutate(hour = dep_time %/% 100) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  filter(n > 5) |>
  mutate(prev_delay = lag(dep_delay, default = first(dep_delay))) |>
  ggplot() +
  geom_point(aes(x = prev_delay, y = dep_delay))


flights |> 
  filter(!is.na(arr_time)) |>
  group_by(origin, dest) |>
  mutate(
    minAirTime = min(air_time, na.rm = TRUE)
  ) |>
  relocate(minAirTime) |>
  ungroup() |>
  mutate(
    difference = air_time - minAirTime 
  ) |>
  relocate(difference, origin, dest, air_time, minAirTime) |>
  arrange(desc(difference))

flights |> 
  mutate(onTime = !is.na(arr_time) & arr_delay <= 0) |>
  group_by(dest) |>
  mutate(countCarrier = n_distinct(carrier, na.rm = TRUE)) |>
  filter(countCarrier > 1) |>
  group_by(carrier) |>
  summarize(n_dest = n_distinct(dest))|>
  arrange(desc(n_dest))

flights |>
  group_by(dest) |>
  summarize(deviation = sd(air_time, na.rm = TRUE), iqr = IQR(air_time, na.rm = TRUE)) |>
  relocate(deviation, iqr) |>
  arrange(desc(deviation), desc(iqr))

flights |>
  filter(dest == "EGE") |>
  group_by(origin, dest) |>
  select(origin, dest, distance, air_time)
  summarize(iqr = IQR(distance))
