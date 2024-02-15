# This is a story about Goblins. And their snot. 
# I have measured the count, diameter, and weight, and classified the colour and moist-factor of the boogers of 8 great goblin houses with varying nose sizes
# With this code we will finally discover whether goober excellence is determined by nose size or goblin house

# sweep the castle (R)
rm(list = ls())

# reference the necessary books
library(tidyverse)
library(googlesheets4)
library(dplyr)

# write a permission scrit to retrieve data from the logbook
gs4_auth(email = "swnemm002@myuct.ac.za")
sheet_names("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0")

# retrieve data from the logbook and render preliminary results according to house
gob <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") 

# remove non-numeric variables and pivot numeric variables to long format against house, then remove observations with a 0
gobnumH <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") %>%
  select(!c("NoseSize", "Colour", "WetDry")) %>% 
  filter(Count > 0) %>%
  pivot_longer(cols = c("Count", "MeanDiameter(mm)", "MeanWeight(g)"), 
               names_to = "variable", values_to = "value")

# make a plot of gobnumH
  ggplot(gobnumH) +
  geom_boxplot(aes(y = value, x = House)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(variable), scales = "free")
  
 
