library(tidyverse)
library(readxl)
library(janitor)
library(compareGroups)


raw_001va <- read_excel("raw_001va.xlsx") %>% 
  clean_names()
