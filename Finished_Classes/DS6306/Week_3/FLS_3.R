#FLS Week 3
#FIFAAAA Part 1
#Create a Player data set that has Left Midfielders and Forwards
summary(fifa)
fifa
lf_lm <- fifa %>% filter(Position == "LF"|  Position == "LM")
view(lf_lm)


lf_lm %>%
  select(Position, Acceleration, Agility) %>%
  ggpairs(aes(color = Position))

#Two-sample TTest 
lf_lm
agimean <- lf_lm %>% group_by(Position) %>% summarize(agimean = mean(Agility), count = n())

agi_LF <- lf_lm %>% filter(Position == 'LF') %>% select(Agility)
agi_LM <- lf_lm %>% filter(Position == 'LM') %>% select(Agility)

str(agi_LF)
str(agi_LM)

summary(agi_LF)
summary(agi_LM)

agi_LF %>% summarise(sd_LF = sd(Agility))
agi_LM %>% summarise(sd_LF = sd(Agility))

t.test(agi_LF,agi_LM)
t.test(agi_LM,agi_LF)

lf_lm %>% ggplot(aes(x = Agility)) +
  geom_histogram(fill = "red") +
  facet_grid(~Position)

view(fifa)
#Exploratory Data Analysis 
#Categorical Variables: Position, Overall(form)
#Continuous Variables: Age, Wage(cleanWage)

summary(fifa$Wage)

#Converted 
fifa_added_forms <- fifa %>% 
  mutate(Form = cut(Overall, breaks = c(0,62,66,71,90,100), labels = c("Poor","Fair", "Good", "Excellent", "Perfect")))

#CurrencyConv Function
currencyconv <- function(x) {
  x <- gsub("€", "", x)
  x <- gsub("K", "000", x)
  as.numeric(x)
}

fifa_new_form_wage <- fifa_added_forms %>%
  mutate(cleanwage = sapply(Wage, currencyconv))

summary(fifa_added_forms$Form)
summary(fifa_new_form_wage$cleanwage)

fifa_new_form_wage %>% ggplot(aes(y = cleanwage, color = Form)) +
  geom_boxplot()

fifa_new_form_wage %>% ggplot(aes(y = cleanwage, fill = Position)) +
  geom_boxplot()+
  facet_wrap(~Position)

fifa_new_form_wage %>% ggplot(aes(y = cleanwage, color = Form)) +
  geom_boxplot(outliers = FALSE)

fifa_new_form_wage %>% ggplot(aes(y = cleanwage, fill = Position)) +
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Position)

fifa_new_form_wage %>% ggplot(aes(x = Age, y = cleanwage)) +
  geom_point()

fifa_new_form_wage %>% ggplot(aes(x = Age, fill = Form)) +
  geom_bar()

fifa_new_form_wage %>% ggplot(aes(x = Age, fill = Form)) +
  geom_bar() +
  facet_wrap(~Position)
