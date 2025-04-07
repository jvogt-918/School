#FLS Resteraunt Data from Baltimore 
library(XML)
library(dplyr)
library(tidyr)
library(stringi)
library(rvest)
library(ggplot2)
library(RCurl)
?rvest
#Collecting, parsing, and knitting data together

brd_doc <- xmlParse(balrestdata)
brd_names <- xpathSApply(brd_doc,"//name",xmlValue)
brd_zipcode <- xpathSApply(brd_doc,"//zipcode",xmlValue)
brd_cd <- xpathSApply(brd_doc,"//councildistrict",xmlValue)
brd = data.frame(brd_names,brd_zipcode,brd_cd)

#How many Sushi Resteraunts are in Baltimore
grep("Sushi", brd$brd_names, ignore.case = TRUE, value = TRUE) 

# How many sushi resteraunts are in CD11/DownTown? 
brd_cd11 <- brd %>% filter(brd$brd_cd == 11)
grep("Sushi", brd_cd11$brd_names, ignore.case = TRUE, value = TRUE)

#Make a Bar plot of Number of Resteraunts in each district
#Making brd_cd numeric to sort better
brd$brd_cd <- as.numeric(as.character(brd$brd_cd))

brd %>% arrange(desc(brd_cd)) %>% ggplot(aes(x = brd_cd, fill = factor(brd_cd))) + 
                 geom_bar(stat = "count") +
  ggtitle("Resteraunts per District") +
  xlab("Council District") +
  ylab("# of Resteraunts") +
  theme(legend.title = element_blank())

#Part 2 APIs!!!!
#Intrinio
# API Key: OjJlMmI0YjkxZWZlN2Y3NTY3MDdiNjY1NDgxOGM4Mzhk
#Trying WDI Cause Intrinio hard
#Okay... Now I'm using Spotify
#Client ID: ae22a90a1bca4617b8277857d8e2ca60
#Client Secrets: f1440baabc0d43ccb8541eac0d26d8f9
#Census API Key: 617b35ad7a2324d281db9872b3faa0f6a857a56a

library(tidyverse)
library(GGally)
library(dplyr)
library(ggplot2)
install.packages("WDI")
library(WDI)
install.packages("spotifyr")
install.packages("httr2")
library(spotifyr)
install.packages("tidycensus")

#Sys.setenv(SPOTIFY_CLIENT_ID = "ae22a90a1bca4617b8277857d8e2ca60")
#Sys.setenv(SPOTIFY_CLIENT_SECRET = "f1440baabc0d43ccb8541eac0d26d8f9")
#access_token <- get_spotify_access_token()
#auth_code <- get_spotify_authorization_code(scope = c("user-library-read", "user-top-read"))
#auth_code <- get_spotify_authorization_code(scope = c("user-library-read", "user-top-read"))#
#artist_data <- get_artist_audio_features("artist_name")
#artist_data
#top_artists <- get_my_top_artists_or_tracks(type = "artists", limit = 5)

library(tidycensus)
census_api_key("617b35ad7a2324d281db9872b3faa0f6a857a56a", install = TRUE)
readRenviron("~/.Renviron")

totalpop_2010 <- get_decennial(
  geography = "state",
  variables = "P001001",
  year = 2010
)

totalpop_2020 <- get_decennial(
  geography = "state",
  variables = "P1_001N" #P1_002N:Male P1_026N:Female
)


total_race_2010 <- get_decennial(geography = "state", 
                                 variables = c("P001001","P001002", "P001003", "P001004","P001005", "P001006", "P001007", "P001008", "P001009"), 
                                 year =2010,
                                 sumfile = "pl")

total_race_2020 <- get_decennial(geography = "state",
                                    variables = c("P1_001N", "P1_002N","P1_003N", "P1_004N", "P1_005N","P1_006N", "P1_007N", "P1_008N", "P1_009N"),
                                    year =2020)

view(total_race_2020)

cenvar2020 <- load_variables(2020, "pl")
cenvar2010 <- load_variables(2010, "pl")
agevar2020 <- load_variables(2020, "acs5")

agevar2020
view(cenvar2010)
cenvar2020

table_p2_2020 <- get_decennial(
  geography = "state",
  table = "P2",
  year = 2020)
view(table_p2_2020)

#Using total_race_2020 and 2010
#Need to translate Variables to corresponding race
total_race_2010 <- total_race_2010 %>% mutate(variable = recode(variable, 'P001001' = 'Total', 'P001002' = "One Race", 'P001003' = 'White','P001004' = 'Black or African American','P001005' = 'Native American/Alaska Native','P001006' = 'Asian','P001007' = 'Native Hawaiian or Pacific Islander','P001008' = 'Other','P001009' = 'Two or More Races'))
total_race_2020 <- total_race_2020 %>% mutate(variable = recode(variable, 'P1_001N' = 'Total', 'P1_002N' = 'One Race','P1_003N' = 'White', 'P1_004N' ='Black or African American', 'P1_005N' = 'Native American/Alaska Native','P1_006N' = 'Asian', 'P1_007N' = 'Native Hawaiian or Pacific Islander', 'P1_008N' = 'Other', 'P1_009N' = 'Two or More Races'))

total_race_2010 <- cbind(total_race_2010, Year = 2010)
total_race_2020 <- cbind(total_race_2020, Year = 2020)

total <- rbind(total_race_2010, total_race_2020)

#Plotting
library(ggplotify)
install.packages("treemap")
library(treemap)
treemap_data <- total %>%
  filter(Year == 2010, variable == "Total")

treemap(treemap_data,
        index = "NAME",
        type = "index",
        vSize = "value",
        title = "State Population in 2010")

total %>% filter(total$variable == "Total") %>%
  treemapify(aes(x = gg, y = "value")) +
  geom_bar(stat = "identity")

treemap_data_2020 <- total %>%
  filter(Year == 2020, variable == "Total")

treemap(treemap_data_2020,
        index = "NAME",
        type = "index",
        vSize = "value",
        title = "State Population in 2020")
