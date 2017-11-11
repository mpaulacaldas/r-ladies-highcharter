library(tidyverse)

comics <- c(
  "DC"     = "https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/dc-wikia-data.csv",
  "Marvel" = "https://raw.githubusercontent.com/fivethirtyeight/data/master/comic-characters/marvel-wikia-data.csv"
  ) %>%
  map(read_csv) %>% 
  map_dfr(~ set_names(.x, tolower(names(.x))), .id = "comic")


new_char_per_year <- comics %>% 
  count(comic, year) %>% 
  filter(between(year, 1939, 2011)) %>% 
  mutate(
    year = factor(year), 
    couleur = ifelse(comic == "DC", "#518cca", "#e23636"),
    titre = ifelse(comic == "DC", 
                   "DC, New Earth continuity", 
                   "Marvel, Earth-616 continuity") 
    ) %>% 
  split(.$comic)

new_fem_per_year <- comics %>% 
  count(comic, year, sex) %>% 
  group_by(comic, year) %>% 
  mutate(share_gender = round(n*100/sum(n), 1)) %>% 
  ungroup() %>% 
  filter(sex == "Female Characters", between(year, 1980, 2011))

save(new_char_per_year, new_fem_per_year,
     file = "data/538comics.RData")
