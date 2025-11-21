Example1 = read.csv("/Users/jvogt/Desktop/School/SMU/Week_1_Async/BusinessSales.csv") 
head(Example1)

#Scatterplot
plot(Example1$ad_tv,Example1$sales, pch = 15, xlab = "TV Advertising $$$", ylab = "Sales $$$", main = "Sales v. Advertising")
abline(h = 55, col = "red", lwd = 5)

#plotting with mpg
plot(mpg$hwy, mpg$cty, pch = 15)
plot(mpg$hwy, mpg$cty, pch = 15, main = "City MPG v. Highway MPG", ylab = "City MPG", xlab = "Highway MPG")


plot(iris$Sepal.Length,iris$Petal.Length)


pairs(~mpg+disp+drat+wt,data=mtcars,
      main="Simple Scatterplot Matrix")


xbars = xbarGenerator(30,1000)
install.packages("xbarGenerator")
xbargenerator



population = rchisq(5000000, 30)
hist(population, col = "blue", main = "Histo of the popu")
mean(population)
sd(population)





a = c("Domain Expertise", "Statistics", "Machine Learning", "Data Visualization", "Computer Science", "Math", "Communication Skills")
b = c(7,6,4,5,7,5,6)

df= data.frame(Skill = a, Level = b)
df

ggplot(data = df, aes(x = Skill, y = Level)) +
  geom_histogram(stat = "Identity", fill = "steelblue")+
  labs(Title = "Johnny's Skill", x = "Skills", y = "Level") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2))

hist(df, main = "Johnny's Profile")


