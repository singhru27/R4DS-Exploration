library(tidyverse)
library(babynames)

str_view(fruit, "berry")

babynames |>
  mutate(vowelCount = str_count(name, "[aeiouAEIOU]")) |>
  group_by(name) |>
  summarize(vowelCount = min(vowelCount)) |>
  arrange(desc(vowelCount)) 
  

x <- "a/b/c/d/e"
x <- str_replace_all(x, "/", "\x")
str_view(x)

x <- "ABCD"

x <- str_replace_all()

"[1-9]"

"\"\'\\\\"

x = "\"\'\\"
str_view(x)
str_detect(x, "\"\'\\\\")

x = "$^$"
str_view(x)
str_detect(x, "\\$\\^\\$")

str_replace(words, "^y", "!")
str_replace(words, "^[^y]", "!")
str_replace(words, "$x", "!")
str_replace(words, "^...$", "!")
str_replace(words, "^.......+$", "!")
str_replace(words, "[aeiou][^aeiou]", "!")
words <- tibble(words)
words <- words |>
  mutate(first = str_sub(words, 1, 1), last = str_sub(words, -1, -1))

words |>
  mutate(words = str_replace(words, "^.", last)) |>
  mutate(words = str_replace(words, ".$", first))

str_view(words, pattern = "^x|x$")
str_view(words, pattern = "^[aeiou]|[^aeiou]$")

words[str_detect(words, "a") &
        str_detect(words, "e") &
        str_detect(words, "i") &
        str_detect(words, "o") &
        str_detect(words, "u")]

words[str_detect(words, "cie")]
str_replace(colors(), "light", "")
