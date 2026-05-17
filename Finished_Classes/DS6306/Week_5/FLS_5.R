#FLS Week 5
#Revisitng BBall Study 
#Use REGEX/String functions to change the height column to create TotalInches
#Also make it Numeric
#Make a histogram of heights for every position color coded
library(stringr)
library(ggthemes)
PlayersBBall <- read_csv("Unit 2/PlayersBBall.csv")

#Changing the Height from x-x to x (Feet to Inches)
PlayersBBall <- PlayersBBall %>%
  separate(col = height, into = c("feet", "inches"), sep = "-")
PlayersBBall$feet <- as.numeric(PlayersBBall$feet)
PlayersBBall$inches <- as.numeric(PlayersBBall$inches)
PlayersBBall$feet <- PlayersBBall$feet * 12
PlayersBBall$TotalInches <- PlayersBBall$feet + PlayersBBall$inches
PlayersBBall

#Plotting
PlayersBBall %>% ggplot(aes(x = TotalInches, fill = position)) +
  geom_histogram(binwidth = 1) +
  xlab("NBA Player's Height in Inches") +
  ylab("Number of Players") +
  ggtitle("Histogram of the Height of NBA Players") +
  guides(fill = guide_legend((title = "Position"))) +
  theme_fivethirtyeight()

#With the FIFA Players Data assess the relationship with HT and WT 
#THan compare data between the LB and LM Position
FIFA_Players <- read_csv("Unit 3/FIFA Players.csv")

#Converting the Height and weight to Numeric numbers to work and run data with
FIFA_Players <- FIFA_Players %>%
  separate(col = Height, into = c("ht_ft", "ht_in", sep = "'"))
FIFA_Players$ht_ft <- as.numeric(FIFA_Players$ht_ft)
FIFA_Players$ht_in <- as.numeric(FIFA_Players$ht_in)
FIFA_Players$ht_ft <- FIFA_Players$ht_ft * 12
FIFA_Players$Height <- FIFA_Players$ht_ft + FIFA_Players$ht_in

FIFA_Players$Weight = str_replace(FIFA_Players$Weight, "lbs", "")
FIFA_Players$Weight = as.numeric(FIFA_Players$Weight)

#Plotting Stuff for Relationships
FIFA_Players %>% ggplot(aes(x = Weight, y = Height)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Weight, y = Acceleration)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Weight, y = Agility)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Weight, y = Stamina)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Height, y = Acceleration)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Height, y = Agility)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Height, y = Stamina)) +
  geom_point()

FIFA_Players %>% ggplot(aes(x = Weight, fill = Position)) +
  geom_histogram()

FIFA_Players %>% ggplot(aes(x = Height, fill = Position)) +
  geom_histogram(binwidth = 1)

library(GGally)
FIFA_Players %>% select(Height, Weight, `Preferred Foot`) %>%
  drop_na(Height, Weight, 'Preferred Foot') %>%
  ggpairs()

FIFA_Players %>% select(Acceleration, Agility, Weight) %>%
  ggpairs()

FIFA_Players %>% select(Acceleration, Agility, Height) %>%
  ggpairs()

FIFA_Players %>% select(Height, Weight, Overall) %>%
  ggpairs()

#Filtering out LB and LM Position and Plotting a scatter plot with posiiton color
FFPlayers_lb_lm <- FIFA_Players %>% filter(Position == "LB" | Position == "LM")
FFPlayers_lb_lm
str(FFPlayers_lb_lm)

FFPlayers_lb_lm %>%
  ggplot(aes(x = Weight, y = Height, color = Position)) +
  geom_point( position = "jitter") +
  geom_smooth(aes(color = Position))

FFPlayers_lb_lm %>%
  ggplot(aes(x = Weight, y = Height)) +
  geom_point( position = "jitter") +
  geom_smooth(aes(color = Position))

FFPlayers_lb_lm %>%
  ggplot(aes(x = Position, y = Weight))+
  geom_boxplot() +
  facet_grid(~Height)

FFPlayers_lb_lm %>%
  ggplot(aes(x = Position, y = Height))+
  geom_boxplot()

#Baby Names
#Making yob2016 into a DataFrame
df <- read.table("Unit 5/yob2016.txt",
                 sep = ";",
                 header = FALSE,
                 col.names = c("Name", "Gender", "Count"))

df$Count <- as.numeric(df$Count)
head(df)
summary(df)
str(df)
view(df)

#Removing Fionayyy and saving dataframe
df$Name %>% str_view("yyy")
df
y2016 <- subset(df, row.names(df) != '212')
y2016

#do the same thing for yob2015 now
df <- read.table("Unit 5/yob2015.txt",
                 sep = ",",
                 header = FALSE,
                 col.names = c("Name", "Gender", "Count"))
df$Count <- as.numeric(df$Count)
head(df)
summary(df)
str(df)
view(df)
y2015 <- df
y2015
anyNA(y2016)
tail(y2015, n= 10)

final <- merge(x = y2015, y = y2016, by = "Name")
final
view(final)
str(final)
final$Total <-  final$Count.x + final$Count.y
view(final)

final <- final[order(final$Total, decreasing = TRUE),]

str(final)
#Top ten names of 2016 and 2017
head(final, n = 10)

final_girl <- final %>% filter(Gender.x == "F")
final_girl
head(final_girl, n = 10)

write.csv(final_girl %>% select(Name,Total) %>% head(n = 10))

top_ten_male <- final %>%
  filter(Gender.x == "M") %>%
  select(Name,Total) %>%
  head(n =10)

top_ten_female  <- final %>%
  filter(Gender.x == "F") %>%
  select(Name,Total) %>%
  head(n =10)

final_girl_sum <- final %>% filter(Gender.x == "F") %>%
  summarise(sum = sum(final$Total))

final_boy_sum <- final %>% filter(Gender.x == "M") %>%
  summarise(sum = sum(final$Total))

final_boy_sum
final_girl_sum
top_ten_female_sum <- top_ten_female %>% summarise(sum = sum(top_ten_female$Total))
top_ten_male_sum <- top_ten_male %>% summarise(sum = sum(top_ten_male$Total))

other_male <- final_boy_sum - top_ten_male_sum
other_female <- final_girl_sum -top_ten_female_sum
other_female <- data.frame(Name = "Other_F", Total = other_female$sum)
other_male <- data.frame(Name = "Other_M", Total = other_male$sum)
str(other_female)

top_ten_female_plus_other <- top_ten_female %>% add_row(other_female)
top_ten_male_plus_other <- top_ten_male %>% add_row(other_male)


top_ten_female_plus_other

library(wordcloud)
