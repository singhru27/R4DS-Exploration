library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    color = "Car type",
    title = "Fuel efficiency generally decreases with engine size",
    subtitle = "Two seaters (sports cars) are an exception because of their light weight",
    caption = "Data from fueleconomy.gov"
  )

labelInfo <- mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1) |>
  mutate (
    driveType = case_when(
      drv == "f" ~ "front wheel drive",
      drv == "r" ~ "rear wheel drive",
      drv == "4" ~ "four wheel drive"
    )
  ) |>
  select(
    displ, hwy, drv, driveType
  )

labelInfo

ggplot (mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) + 
  geom_label_repel(
    data = labelInfo,
    aes(x = displ, y = hwy, label = driveType),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  ) + 
  theme(legend.position = "none")

mpgOutliers <- mpg |>
  filter(hwy > 40 | (hwy > 20 & displ > 5))
mpgOutliers

ggplot (mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  geom_point(data = mpgOutliers, color = "red", size = 5) +
  geom_label_repel(
    data = mpgOutliers,
    mapping = aes(label = model),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  )

trend_text <- "Larger engine sizes tend to have lower fuel economy." |>
  str_wrap(width = 30)
trend_text
#> [1] "Larger engine sizes tend to\nhave lower fuel economy."
#> 
labels <- tribble(
  ~displ, ~hwy, ~label, ~vjust, ~hjust,
  Inf, Inf, "Top right", "top", "right",
  Inf, -Inf, "Bottom right", "bottom", "right",
  -Inf, Inf, "Top left", "top", "left",
  -Inf, -Inf, "Bottom left", "bottom", "left"
)

ggplot (mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  annotate(
    geom = "label", x = 3.5, y =38,
    label = trend_text,
    hjust = "left",
    color = "red"
  ) + 
  annotate(
    geom = "segment",
    x = 3, y = 35, xend = 5, yend = 25, color = "red",
    arrow = arrow(type = "closed")
  ) + 
  geom_text(data = labels, aes(label = label, hjust = hjust, vjust = vjust)) + 
  annotate(
    geom = "point", x = mean(Inf, -Inf), y =mean(Inf, -Inf),
    color = "red"
  ) 


newLabel = tribble(
  ~displ, ~hwy, ~class, ~label,
  Inf, Inf, "compact", "Test Label"
)
ggplot (mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  facet_wrap(~class) + 
  geom_text (
    data = newLabel,
    mapping = aes (label = label), hjust = "right", vjust = "top",
    
  )
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) + 
  geom_point()+
  scale_y_continuous(breaks = seq(15, 40, by = 5), labels = NULL) + 
  scale_x_continuous(labels = NULL) + 
  scale_color_discrete(labels = c("4" = "4-Wheel", "f" = "front","r" = "rear" ))


ggplot(diamonds, aes(x = cut, fill = clarity)) + 
  geom_bar(position = "dodge")

presidential |> 
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id)) + 
  geom_point() + 
  geom_segment(aes(xend = end, yend = id)) + 
  scale_x_date(name = NULL, breaks = presidential$start, date_labels = "'%y")

presidential |> 
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) + 
  geom_point() + 
  geom_segment(aes(xend = end, yend = id)) + 
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3"))

ggplot (mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth() + 
  coord_cartesian(xlim = c(5, 6), ylim = c(10, 25))

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()


presidential |> 
  mutate(id = 33 + row_number()) |>
  ggplot(aes(x = start, y = id, color = party)) + 
  geom_point() + 
  geom_segment(aes(xend = end, yend = id)) + 
  scale_x_date(breaks = presidential$start, date_breaks = "4 years", date_labels = "'%y") + 
  scale_color_manual(values = c(Republican = "#E81B23", Democratic = "#00AEF3")) + 
  labs(
    title = "Presidential Terms over Time"
  ) + 
  geom_text(
    mapping = aes(label = name, vjust = "bottom"), nudge_y = .2
    )

p1 <- ggplot (mpg, aes(x = displ, y = hwy)) + 
  geom_point()

p2 <- ggplot (mpg, aes(x = drv, y = hwy)) + 
  geom_point()

p1 + p2
