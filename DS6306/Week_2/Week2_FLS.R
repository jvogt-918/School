#Importing PlayersBBall Dataset
library(readr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(tidyverse)
PlayersBBall <- read_csv("Unit 2/PlayersBBall.csv")
View(PlayersBBall)

#Visually represent number of players in each position
PlayersBBall[PlayersBBall$name != "George Karl",] %>% ggplot(aes(x= position)) +
  geom_bar() +
  ggtitle("Number of Players by Position since 1950") +
  xlab("Position") +
  ylab("Player Count") + 
  theme_par()


#Investigate the distribution of the weight of Centers between the weight of Forwards
PlayersBBall[PlayersBBall$position != "G",] %>% ggplot(aes(x = weight, fill = position  )) +
  geom_histogram() +
  theme_par()

PlayersBBall$position <- reorder(PlayersBBall$position, PlayersBBall$weight, FUN = median)
PlayersBBall[PlayersBBall$name != "George Karl",] %>% ggplot(aes(x = weight, y = position)) +
  geom_boxplot(outliers = FALSE) +
  ggtitle("Summary of Weight Distribution for Forwards and Centers") +
  xlab("Weight") +
  ylab("Position")


  
#Investigate the distribution of the height of Centers and the height of Forwards
PlayersBBall[PlayersBBall$position != "G",] %>% ggplot(aes(x = height, y = height, color = position)) +
  geom_point()+
  geom_jitter()+
  ggtitle("Summary of Height Distribution for Forwards and Centers") +
  xlab("Weight") +
  ylab("Position")

view(PlayersBBall)
#GSUB did Not work lol
player_ht <- gsub("-", " ", PlayersBBall$height)

#function made to convert height(6-10) to inches
ft_to_inches <- function(height_str) {
  parts <- strsplit(height_str, "-") [[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- feet * 12 + inches
  return(total_inches)
}

Convertedht_PlayersBBall <- PlayersBBall %>% mutate(new_height = sapply(height,ft_to_inches))
Convertedht_PlayersBBall

Convertedht_PlayersBBall$position <- reorder(Convertedht_PlayersBBall$position, Convertedht_PlayersBBall$new_height, FUN = median)
Convertedht_PlayersBBall[Convertedht_PlayersBBall$name != "George Karl",] %>% ggplot(aes(x = new_height, y = position)) +
  geom_boxplot(outliers = FALSE) +
  ggtitle("Boxplot of Player's Height by Position") +
  xlab("Height in Inches") +
  ylab("Position")

Convertedht_PlayersBBall[Convertedht_PlayersBBall$position != "G",] %>% ggplot(aes(x = new_height, y =new_height, color = position)) +
  geom_point()+
  geom_jitter()+
  ggtitle("Summary of Height Distribution for Forwards and Centers") +
  xlab("Weight") +
  ylab("Position")
  

Convertedht_PlayersBBall$new_height

#Investigate the if the distribution of height is different between any of the position

Convertedht_PlayersBBall[Convertedht_PlayersBBall$name != "George Karl",] %>% ggplot(aes(x = new_height)) +
  geom_bar() +
  ggtitle("Summary of Height Distribution") +
  xlab("Height") +
  ylab("Count of Players at a Specific Height") +
  theme_par() +
  facet_wrap(~position)

#Investigate the relationship between height and weight, how does height change as the weight change
Convertedht_PlayersBBall %>% ggplot(aes(x = weight, y = new_height)) +
  geom_point(position = "jitter") +
  geom_smooth() +
  ggtitle("Height & Weight of Every NBA Player since 1950") +
  xlab("Player's Weight") +
  ylab("Player's Height")

#Is there a H/W relationship for each position
Convertedht_PlayersBBall[Convertedht_PlayersBBall$name != "George Karl",] %>% ggplot(aes(x = weight, y = new_height, color = position)) +
  geom_point() +
  geom_smooth(color = "black") +
  ggtitle("Height & Weight of Every NBA Player since 1950") +
  xlab("Player's Weight") +
  ylab("Player's Height") +
  facet_wrap(~position)

#Investigate the claim that the height of players have increased over the years

Convertedht_PlayersBBall %>% ggplot(aes(x = year_start, y = new_height)) +
  geom_point(size = 1, position = "jitter") +
  geom_smooth(color="red", linewidth = 3) +
  ggtitle("Scatterplot for Height of Players over the Years") +
  xlab("Year") +
  ylab("Height in inches") +
  theme_par()


Convertedht_PlayersBBall %>% ggplot(aes(x = year_start)) +
  geom_line(aes(y = new_height)) +
  ggtitle("Lollipop Chart for Height of Players over the Years") +
  xlab("Year") +
  ylab("Height in inches") +
  theme_par()


#Create a 3D plot of Height v. Weight v. Year and color code the position
library(plotly)
H_W_Y <- plot_ly(Convertedht_PlayersBBall, x = ~weight, y = ~new_height, z = ~year_start, color = ~position, sizes=c(1,1)) %>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = "Player's Weight"),
                      yaxis = list(title = "Player's Height"),
                      zaxis = list(title = "Debut Year")))
  
H_W_Y


#Go to the website (found in PowerPoint) pick one and provide insight
######Time Series########################
Convertedht_PlayersBBall %>% ggplot(aes(x=year_start, y = new_height))
geom_boxplot()

print(med_ht_by_year)
summar

g <- Convertedht_PlayersBBall[Convertedht_PlayersBBall$name != "George Karl",] %>% ggplot(aes(x = new_height,y = new_height)) +
  geom_point(position = "jitter") +
  xlab("Height in Inches") +
  ylab("Height in Inches") +
  ggtitle("Height Over the Years")

g_animated <- g +
  transition_states(year_start) +
  enter_fade() +
  exit_fade()

animate(g_animated, fps = 1)

med_ht_by_year <- Convertedht_PlayersBBall %>%
  group_by(year_start) %>%
  summarise(median_height = median(new_height))

med_ht_by_year %>% ggplot(aes(x=year_start, y = median_height)) +
  geom_line() +
  ggtitle("Yearly Time Series of Median NBA Player's Heights") +
  xlab("Debut Year") +
  ylab("Median Height in Inches") +
  theme_par()

View(Convertedht_PlayersBBall)



install.packages("gganimate")

#Import Education Inncome data and test the claim that the mean/median of incomes increase as the education level rises.
Education_Income <- read_csv("Unit 2/Education_Income.csv")
View(Education_Income)
summary(Education_Income)
str(Education_Income)

#Reordering Based off the Median of the of the Income, Helps 
Education_Income$Educ <- reorder(Education_Income$Educ, Education_Income$Income2005, FUN = median)

Education_Income %>% ggplot(aes(x = Income2005, y = Educ)) +
  geom_boxplot(outliers = FALSE) +
  ggtitle("Boxplot of Salaries based off of Education Level") +
  xlab("Income in Dollars") +
  ylab("Education Level") +
  theme_economist()
