# This is a story about Goblins. And their snot. 
# I have measured the count, diameter, and weight, and classified the color and moist-factor of the boogers of 8 great goblin houses with varying nose sizes
# With this code we will finally discover whether goober excellence is determined by nose size or goblin house

# sweep the castle (R)
rm(list = ls())

# grab the necessary books
library(tidyverse)
library(googlesheets4)
library(dplyr)
library(googledrive)

# This is the code that I have used with access to my google sheets
# I have included code for reading in the files with excel beneath (line 18) as I have included an excel document
# You can replace the first line of code in the chunk starting from line 25 with the code provided beneath
### For the excel files
#gob <- read_xlsx("data/goblinboogers.xlsx", sheet = "Sites")

# write a permission to retrieve data from the googlesheets and see what sheets there are
gs4_auth(email = "swnemm002@myuct.ac.za")
sheet_names("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0")

# retrieve data from the googlesheet
gob <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") 

# remove non-numeric variables and pivot numeric variables to long format against HOUSE, then remove observations with a 0
gobnumH <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") %>%
  select(!c("NoseSize", "Colour", "WetDry")) %>% 
  filter(Count > 0) %>%
  pivot_longer(cols = c("Count", "MeanDiameter(mm)", "MeanWeight(g)"), 
               names_to = "variable", values_to = "value")

# make a plot of gobnumH (HOUSE)
ggplot(gobnumH) +
  geom_boxplot(aes(y = value, x = House, fill = House)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(variable), scales = "free") +
  scale_fill_brewer(palette = "BuPu")

  
# now making a dataframe with observations classified by NOSE SIZE
gobnumS <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") %>%
    select(!c("House", "Colour", "WetDry")) %>% 
    filter(Count > 0) %>%
    pivot_longer(cols = c("Count", "MeanDiameter(mm)", "MeanWeight(g)"), 
                 names_to = "variable", values_to = "value")
  
# and plot those data for NOSE SIZE
  ggplot(gobnumS) +
    geom_boxplot(aes(y = value, x = NoseSize, fill = NoseSize)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(vars(variable), scales = "free") +
    scale_fill_brewer(palette = "BuPu")

# grab data for WET AND DRY snot  
gobnumWD <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") %>%
    select(!c("House", "NoseSize", "Colour")) %>% 
    filter(Count > 0) %>%
    pivot_longer(cols = c("Count", "MeanDiameter(mm)", "MeanWeight(g)"), 
                 names_to = "variable", values_to = "value")
  
# and plot those data for WET AND DRY snot
ggplot(gobnumWD) +
    geom_boxplot(aes(y = value, x = WetDry, fill = WetDry)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(vars(variable), scales = "free") +
    scale_fill_brewer(palette = "BuPu")

# grab data for  COLOR 
gobnumS <- read_sheet("https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", sheet = "GoblinBoogers") %>%
  select(!c("House", "NoseSize", "WetDry")) %>% 
  filter(Count > 0) %>%
  pivot_longer(cols = c("Count", "MeanDiameter(mm)", "MeanWeight(g)"), 
               names_to = "variable", values_to = "value")

# and plot those data for COLOR
ggplot(gobnumS) +
  geom_boxplot(aes(y = value, x = Colour, fill = Colour)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(vars(variable), scales = "free") +
  scale_fill_brewer(palette = "BuPu")

# download raw data, convert to excel, and put it into the folder "Data"
drive_auth(email = "swnemm002@myuct.ac.za")
drive_download(file = "https://docs.google.com/spreadsheets/d/1URCJQLwtJAjhm4-3__Ckocf7O76nBu-JjYJZP2a3ozA/edit#gid=0", 
               path = "data/goblinboogers.xlsx", overwrite = TRUE)
