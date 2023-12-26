library(tidyverse)
gss_cat

ggplot(data = gss_cat) +
  geom_bar(mapping = aes(x = relig)) + 
  coord_flip()

gss_cat |>
  filter(!str_detect(denom, "(Not applicable)") & !str_detect(denom, "(No answer)")) |>
  count(relig, denom)

gss_cat |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_collapse(
      partyid, 
      "other" = c("No answer", "Don't know", "Other party"),
      "rep" = c("Strong republican", "Not str republican"),
      "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
      "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  group_by(year, partyid) |>
  summarize(count = n()) |>
