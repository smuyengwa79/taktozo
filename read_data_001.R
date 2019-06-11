library(tidyverse)
library(readxl)
library(janitor)
library(compareGroups)


raw_001 <- read_excel("raw_001va.xlsx") %>% 
  clean_names()

yvars <- names(raw_001[75:90])

raw_001 %>% 
  group_by(a_a2) %>% 
  table1(phc_d2, phc_d3, 
         phc_d4, phc_d5, 
         phc_d6, phc_d7)




dataset2 <- read_excel("raw_002V1.xlsx") 
dt2 <- dataset2 %>% 
  select(District, starts_with("ttl_"))

a <- descrTable(District ~ ., dt2, hide.no="no", 
                show.all = TRUE, 
                include.miss = TRUE)
export2csv(a, file = "eduHealthCosts.csv")

livelihoods <- dataset2 %>% 
  select(District, 137: 179) 
livelihoods <- livelihoods %>% 
  select(-starts_with("How"))

livelihoods <- livelihoods %>%
  mutate_at(.vars = vars(2:24), 
            .funs = funs(ifelse(. == "Yes", 1, 0)))

livelihoods <- livelihoods %>%
  replace(is.na(.), 0) %>%
  mutate(LivelihoodDiversity = rowSums(.[2:24]))
