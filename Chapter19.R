library(tidyverse)
library(nycflights13)

weather <- weather |>
  relocate(time_hour)

weather

flights <- flights |>
  mutate(sched_dep_time = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100))

flights_delay <- flights |>
  mutate(sched_dep_time = floor_date(sched_dep_time, unit = "hour"))

flights_delay |>
  group_by(origin, sched_dep_time) |>
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE), n = n()) |>
  left_join(weather, join_by(origin == origin, sched_dep_time == time_hour)) |>
  arrange(desc(mean_delay)) |>
  head(48)

top_dest <- flights2 |>
  count(dest, sort = TRUE) |>
  head(10)  

top_dest
flights2 |> semi_join(top_dest)

flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)

flights2 |> anti_join(weather, join_by(time_hour == time_hour, origin == origin)) |>
  count(time_hour)

missingFlights <-
  flights |> 
  anti_join(planes, join_by(tailnum)) |>
  count(carrier)
missingFlights

planes |>
  left_join(flights, join_by(tailnum)) |>
  distinct(tailnum, carrier)

flights |>
  left_join(airports, join_by(origin == faa)) |>
  left_join(airports, join_by(dest == faa))



flights |>
  group_by(dest) |>
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE)) |>
  ungroup() |>
  left_join(airports, join_by(dest == faa)) |>
  ggplot(aes(x = lon, y = lat, color = avg_delay, size = avg_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
    

