#Cleaning of SONGS visual survey fish data into smaller data sizes
#created by: Jonathan Huang 2023-02-11



#load libraries 
library(tidyverse)
library(here)


#import data
phase_2_3 <- read.csv(here("data", "songs_fish_leng_abd","reef_ts_fish_size_and_abundance-2022-11-29_08-33-37.csv"))
phase_1 <- read.csv(here("data", "songs_fish_leng_abd","reef_ts_fish_size_and_abundance_2000-to-2008_20230412.csv")) 
#-99999 denotes not recorded or not avaliable
#count can be 0 but still have a total_length, need clarification

#filter to have target species - PACL, OXCA, PANE, CHPU,EMJA, SEPU
songs <- bind_rows(phase_1,phase_2_3) %>% 
  filter(species_code %in% c("PACL", "OXCA", "PANE",
                             "CHPU", "EMJA", "SEPU"),
                              count != 0) %>% 
write.csv(here("data","songs_clean.csv"))

#Analysis

view(songs)
glimpse(songs)
unique(songs$phase_built_code)

