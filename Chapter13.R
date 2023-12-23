library(tidyverse)
library(nycflights13)

flights |> count(dest, sort = TRUE)

