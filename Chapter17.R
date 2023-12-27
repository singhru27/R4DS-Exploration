library(tidyverse)
library(nycflights13)
?read_csv
ymd("2017-01-31")
#> [1] "2017-01-31"
mdy("January 31st, 2017")
#> [1] "2017-01-31"
dmy("31-Jan-2017")
#> [1] "2017-01-31"
?ymd
ymd(c("2010-10-10", "bananas"))


flights_dt <-flights |>
  mutate(dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100 ))

flights_dt
flights_dt |>
  ggplot() +
  geom_freqpoly(mapping = aes(x = hour, group = ))

flights_new <- flights |>
  mutate(dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100 ),
         sched_dep_time = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100)) |>
  select(dep_time, sched_dep_time, dep_delay)

flights_new |>
  mutate(difference = dep_delaysched_dep_time - dep_time)

flights_new <- flights |>
  mutate(dep_time = make_datetime(year, month, day, dep_time %/% 100, dep_time %% 100 ),
         sched_dep_time = make_datetime(year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100),
         arr_time = make_datetime(year, month, day, arr_time %/% 100, arr_time %% 100))

flights_new |>
  mutate(weekday = wday(sched_dep_time, label = TRUE), .before = 1) |>
  group_by(weekday, label = TRUE) |>
  summarize(mean_delay = mean(dep_delay, na.rm = TRUE)) |>
  ggplot() + 
  geom_col(mapping = aes(x = weekday, y = mean_delay))

diamonds |>
  ggplot() +
  geom_histogram(mapping = aes(x = carat))


flights_new |>
  ggplot() +
  geom_histogram(mapping = aes(x = year(sched_dep_time)))

levels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60")
flights_new |>
  mutate(delayed = dep_delay > 0, 
         newMinute = minute(dep_time) %/% 10 *10 ,
         minutes = factor(newMinute, 
                          levels = c(0, 10, 20, 30, 40, 50), 
                          labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60")
                          ),
         .before = 1) |>
  group_by(newMinute) |>
  summarize(mean_delay = mean(dep_delay)) |>
  ggplot() +
  geom_point(mapping = aes(x = newMinute, y = mean_delay))

ymd("2015-01-01") + months(0:11)

floor_date(today(), "year")+ months(0:11)

age <- function(bday) {
  interval <- bday %--% today()
  return interval / years(1)
}
