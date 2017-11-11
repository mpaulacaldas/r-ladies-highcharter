library(tidyverse)
library(stringr)
library(readxl)

# Import

candy0 <- read_excel("raw/candyhierarchy2017.xlsx", col_types = "text") %>% 
  select(-X__1)

# Clean each group of variables separately

media_qs <- candy0 %>% 
  select(`Internal ID`, starts_with("Q12")) %>% 
  gather("q12", "a12", -`Internal ID`) %>% 
  mutate_at("a12", as.numeric) %>% 
  group_by(`Internal ID`) %>% 
  mutate(a12_submitted = 1*!all(is.na(a12))) %>%
  ungroup() %>% 
  mutate(
    media = ifelse(
      a12 == 1 & a12_submitted == 1, 
      str_replace_all(q12, "Q12\\: MEDIA \\[|\\]", ""), 
      NA)
    ) %>% 
  filter(!is.na(media) | (is.na(media) & a12_submitted == 0)) %>% 
  distinct(`Internal ID`, media) 

candy_qs <- candy0 %>% 
  select(`Internal ID`, starts_with("Q6"))%>% 
  gather("candy", "feelies", -`Internal ID`) %>% 
  mutate_at("candy", str_replace, pattern = "Q6 \\| ", replacement = "") %>% 
  filter(!is.na(feelies)) 

other_qs <- candy0 %>% 
  select(`Internal ID`, JOY = `Q7: JOY OTHER`, DESPAIR =`Q8: DESPAIR OTHER`) %>% 
  gather("feelies", "candy", -`Internal ID`) %>% 
  filter(!is.na(candy)) %>% 
  mutate(other_candy = 1)

demo_qs <- candy0 %>% 
  select(-matches("^Q[6-8]|^Q12")) %>%
  set_names(
    c(
      "Internal ID",
      "trick_or_treater",
      "gender",
      "age",
      "country",
      "location",
      "comments",
      "dress",
      "day",
      "click_coords"
    )
  )


# Bind & merge everything

candy <- bind_rows(candy_qs, other_qs) %>% 
  mutate_at("other_candy", coalesce, 0) %>% 
  left_join(media_qs) %>% 
  left_join(demo_qs) %>% 
  rename(id = `Internal ID`)

save(candy, file = "data/candy.RData")
  
