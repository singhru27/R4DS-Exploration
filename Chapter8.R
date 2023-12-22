library(tidyverse)
library(nycflights13)
library(lvplot)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "white", size = 5) +
  geom_point(aes(color = drv), size = 3) +
  geom_smooth(aes(linetype = drv), se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl)

ggplot(mpg) + 
  geom_point(aes(x = drv, y = cyl))

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(mpg, aes(x = displ)) + 
  geom_histogram() + 
  facet_grid(drv ~ .)

ggplot(mpg, aes(x = displ)) + 
  geom_histogram() +
  facet_grid(. ~ drv)


newDiamonds <- diamonds |> 
  group_by(cut) |>
  summarize(
    maximum = max(depth),
    minimum = min(depth),
    median = median(depth)
  )
newDiamonds

ggplot(newDiamonds) + 
  geom_pointrange(aes(y = median,x = cut, ymin = minimum, ymax = maximum))


ggplot(diamonds, aes(x = cut, fill = color)) + 
  geom_bar()


ggplot(mpg, aes(x = drv, color = class)) + 
  geom_bar(fill = NA, position = "dodge")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "identity")  

ggplot(mpg, aes(class, hwy, color = drv)) + geom_boxplot()


ggplot(diamonds, aes(x = "", fill = cut)) +
  geom_bar() + 
  coord_polar(theta = "y")

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

smaller <- diamonds |> 
  filter(carat < 3)

ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

glimpse(diamonds)

ggplot (diamonds, aes(x = z)) +
  geom_histogram()

unusual <-
  diamonds |> filter(y < 3 | y > 20) |>
  select(price, x, y, z) |>
  arrange(y)

ggplot(unusual, aes (x = y)) + 
  geom_histogram()

ggplot (diamonds, aes(x = price)) +
  geom_histogram(binwidth = 1000)

diamondsCount <- 
  diamonds |> 
  group_by(carat) |>
  summarise(n = n())
diamondsCount |> filter(carat == .99 | carat == 1.00)

diamondsModified <- 
  diamonds |>
  mutate(y = if_else(y > 3 & y < 20, y, NA))

diamondsModified
ggplot (diamondsModified, aes(x = y)) +
  geom_histogram(binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))

ggplot (diamondsModified, aes(x = x, y = y)) + 
  geom_point()

modifiedFlights <- flights |>
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + (sched_min / 60)
  )

modifiedFlights

ggplot (modifiedFlights, aes (x = sched_dep_time)) + 
geom_freqpoly(aes(color = cancelled), binwidth = 1/4) + 
  facet_grid(cancelled ~ ., scales = "free")
  
ggplot (modifiedFlights, aes (x = sched_dep_time)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4) + 
  facet_grid(~cancelled, scales = "fixed")

ggplot(diamonds, aes(x = price, y = after_stat(density))) + 
  geom_freqpoly (
    aes (color = cut),
    binwidth = 500, 
    linewidth = .75
  )

ggplot (diamonds, aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot (diamonds, aes(x = cut, y = price)) + 
  geom_lv()

ggplot (mpg, aes(x = fct_reorder(class, hwy, median), y = hwy)) + 
  geom_boxplot()

ggplot (modifiedFlights, aes (x = sched_dep_time, y = cancelled)) + 
  geom_boxplot() + 
  coord_flip()

ggplot (modifiedFlights, aes (x = cancelled, y = sched_dep_time)) + 
  geom_boxplot() 

glimpse(diamonds)
ggplot(diamonds, aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")

ggplot(diamonds, aes(x = color, y = carat)) +
  geom_violin()

ggplot(diamonds, aes(x = carat)) +
  geom_histogram() +
  facet_grid(.~ color)
  
ggplot(diamonds, aes(x = carat, color = color)) +
  geom_freqpoly()

diamondCount <- 
  diamonds |> 
  count(color, cut)

ggplot(diamondCount, aes(x = color, y = cut)) +
  geom_tile(aes(fill = n)) 

diamondCount

diamondCount <- 
  diamondCount |> 
  group_by(color) |>
  mutate(prop = n / sum(n))

ggplot(diamondCount, aes(x = color, y = cut)) +
  geom_tile(aes(fill = prop))

ggplot(diamonds, aes(x = color)) +
  geom_bar(aes(fill = cut))

flightDelays <-
  flights |>
  group_by(month, dest) |>
  summarise(avgDepDelay = mean(dep_delay, na.rm = TRUE))
flightDelays

ggplot(flightDelays, aes(x = month, y = dest)) +
  geom_tile(aes(fill = avgDepDelay))



ggplot (diamonds, aes(x = carat, y = price)) +
  geom_point(position = "jitter", alpha = 1/100)


ggplot (diamonds, aes(x = carat, y = price)) +
  geom_bin2d()

ggplot (diamonds, aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut_width(carat, 0.1)))

ggplot (diamonds, aes(x = price, color = cut_number(carat, 5))) + 
  geom_freqpoly()

ggplot (diamonds, aes(x = price, y = carat)) +
  geom_boxplot(aes(group = cut_number(price, 10)))

ggplot (diamonds, aes(x = cut_number(carat, 10), y = price, color = cut)) + 
  geom_boxplot()

ggplot (diamonds, aes(x = cut_number(carat, 10), y = price)) +
  geom_boxplot(aes())


diamonds |> 
  filter(x >= 4) |> 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
