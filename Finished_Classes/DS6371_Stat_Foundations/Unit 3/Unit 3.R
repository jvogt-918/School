#Two Sided Ttest 
t.test(x = twitch$Placebo, y = twitch$Ritalin, paired  = TRUE)


#question 3
t.test(EducationData$Income2005 ~ EducationData$Educ)



library(ggplot2)
# Create histogram for each education group
ggplot(EducationData, aes(x = Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram of 2005 Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()

EducationData$log_Income2005 <- log(EducationData$Income2005)

EducationData

# Create histogram for each education group
ggplot(EducationData, aes(x = log_Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram Log'd Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()

t.test(EducationData$log_Income2005 ~ EducationData$Educ, var.equal = TRUE)


#Unit 3 HW

fired_samoan = c(34, 37, 37, 38, 41, 42, 43, 44, 44, 45, 45, 45, 46, 48, 49, 53, 53, 54, 54, 55, 56)
nonfired_samoan = c(27, 33, 36, 37, 38, 38, 39, 42, 42, 43, 43, 44, 44, 44, 45, 45, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 51, 51, 52, 54)

t.test(x = fired_samoan, y = nonfired_samoan, conf.int = .95, var.equal = FALSE, alternative = "two.sided")


ggplot(EducationData, aes(x = Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram Log'd Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()
EducationData$log_Income2005 <- log(EducationData$Income2005)

ggplot(EducationData, aes(x = log_Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram Log'd Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()

tapply(EducationData$log_Income2005, EducationData$Educ, sd, na.rm=TRUE)

t.test(EducationData$log_Income2005 ~ EducationData$Educ, var.equal = FALSE)
