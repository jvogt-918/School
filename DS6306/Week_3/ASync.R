#Unit 3 Data Transformation, wrangle
#packages
library(tidyverse)
library(GGally)


mpg
#fitering with a a double = sing
mpg[mpg$class == "compact",] = mpg %>% filter(class == "compact")


#Adding historgram
mpg[mpg$class == "compact",] %>% ggplot(aes(x = cty)) +
geom_histogram() + ggtitle ("City MPG")

#arranging

mpg %>% arrange(manufacturer) %>% 
  print(n=30)

#arranging with two variables
mpg %>% arrange(manufacturer,cty) %>% 
  print(30)

mpg %>%
  arrange(manufacturer)



#chooses the columns class, city, hwy

mpg %>%
  select(class, cty, hwy) %>%
  ggpairs(aes(color = class))


FIFA_Players %>%
  select(Finishing, BallControl, ShotPower) %>%
  ggpairs()


FIFA_Players %>%
  filter("Preferred Foot" == "Right" | "Preferred Foot" == "Left") %>%
  select("Finishing", "BallControl", "ShotPower", "Preferred Foot") %>%
  ggpairs(aes(color = "Preferred Foot"))

FIFA_Players %>%
  filter(Preferred.Foot == "Right" | Preferred.Foot == "Left") %>% 
  select(Finishing, BallControl, ShotPower, Preferred.Foot) %>% 
  ggpairs(aes(color = Preferred.Foot))


#two variable, group_by usage
mpg %>% 
  group_by(class, year) %>% summarize(meanCTY = mean(cty), count = n())


#stat = identity adds the values of the y value per class (x level)
mpg %>%
  ggplot(aes(x =class, y = cty)) +
  geom_bar(stat = "identity")
                                   
mpg %>%
  group_by(class) %>%
  summarize(sum(cty))

fifa %>%
  group_by(Position) %>%
  summarize(meanBC = mean(BallControl), count = n()) %>%
  print(n = 28)

fifa %>%
  filter(!is.na(BallControl)) %>%
  group_by(Position) %>%
  summarize(meanBC = mean(BallControl), count = n()) %>%
  print(n=28)
  
#factor
#Dataframe for the Example
age = c(22,21,NA,24,19,20,23)
yrs_math_ed = c(4,5,NA,2,5,3,5)
names = c("Mary","Martha","Rosy","Kim","Kristen","Amy","Sam")
subject = c("English","Math",NA,"Sociology","Math","Music","Dance")
df = data.frame(Age = age, Years = yrs_math_ed,Name = names, Major = subject)
df

summary(mpg$cyl)

mpg %>%
  ggplot(aes(x = hwy, y = cty, color = cyl)) +
  geom_point()

mpg %>%
  ggplot(aes(x = hwy, y = cty, color = as.factor(cyl))) +
  geom_point()


cylfact = factor(mpg$cyl)
cylfact
levels(cylfact) = c("Four","Five","Six","Eight")
cylfact

cylfact = factor(mpg$cyl, labels = c("Four","Five", "Six", "Eight" ))
cylfact


mpg %>%
  mutate(cyl_factor = cylfact) %>%
  ggplot(aes(x=hwy,  y = cty, color = cyl_factor)) +
  geom_point()

install.packages("naniar")
library(naniar)
gg_miss_var(diamonds)
sapply(diamonds, function(x) sum(is.na(x)))
diamonds


diamonds %>% count(diamonds$z > 4)
er <- diamonds %>% ggplot(aes(x = z, y = price)) +
  geom_point(position = "jitter")

diamonds %>% ggplot(aes(x = z)) +
  geom_histogram()

diamonds %>% group_by(z) %>% summarize(count = n()) %>% print(n = 365)

p = diamonds %>%
  count(color,cut) %>%
  
exp