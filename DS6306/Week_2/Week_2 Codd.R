library(tidyverse)
## Mapping | Aesthetics
mpg %>%
  ggplot(mapping = aes(x = hwy, y = cty)) +
  geom_point()

mpg %>%
  ggplot(mapping = aes(x = hwy)) +
  geom_histogram()

mpg %>%
  ggplot(mapping = aes(x = class)) +
  geom_bar()

mpg %>% ggplot(mapping = aes(x = class, y = ..prop.., group = 1)) +
  geom_bar()

### AES: color
mpg %>% ggplot(aes(x = hwy, y = cty,color = mpg$drv)) + geom_point()

# color = "blue" is not a mapping of a variable
mpg %>% ggplot(aes(x = hwy, y = cty, color = "blue")) + 
  geom_point()

#note that color is not in an ”aes”
# function we are setting them manually.
mpg %>% ggplot(aes(x = hwy, y = cty)) + 
  geom_point(color = "blue")

# note now we are ”mapping” fill to a variable (drv).
mpg[mpg$class == "compact",] %>% ggplot(aes(x = cty, fill = drv)) + 
  geom_histogram(color = "black") 

#note that fill and color are not in an ”aes” function we are setting them manually.
# We are not “mapping” variables to colors… rather just setting the color.  
mpg[mpg$class == "compact",] %>% ggplot(aes(x = cty)) + 
  geom_histogram(fill = "blue" , color = "black") 

# note now we are ”mapping” fill to a variable (drv).
mpg %>% ggplot(aes(x = class, y = cty,fill = drv)) + geom_boxplot(color = "blue")

#note that fill and color are not in an ”aes” function we are setting them manually.
# We are not “mapping” variables to colors… rather just setting the color.  
mpg %>% ggplot(aes(x = class, y = cty)) + geom_boxplot(color = "blue", fill = "black") 

mpg %>% ggplot(aes(x = hwy, y = cty, color = class, linetype = drv)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~class)


#Labels, Labeling, and more
ggplot(data=mpg, aes(x = class, y = cty)) +
  geom_boxplot()+
  ggtitle("Boxplot of Cty MPG") 

#Facets 
mpg %>% ggplot(aes(x = hwy, y = cty, color = class, linetype = drv)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~class)

mpg %>% ggplot(aes(x = hwy, y = cty, color = class, linetype = drv)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(drv~class)

ggplot(data = mpg, aes(fill = drv)) +
  geom_histogram(mapping = aes(x = cty)) +
  facet_grid(drv~class)

mpg %>% ggplot(aes(x= class)) +
  geom_bar(stat = "count")

mpg %>% ggplot(aes(x= class, y = cty)) +
  geom_bar(stat = "identity")

#coord
mpg %>% ggplot(mapping = aes(x = class, fill = class)) + 
  geom_bar() + 
  coord_flip()
mpg %>% ggplot(mapping = aes(x = class, fill = class))+ 
  geom_bar() + 
  coord_polar()

install.packages("maps")
library(maps)

usa = map_data("usa")
p <- ggplot() +
  geom_polygon(data =usa,aes(x = long, y = lat, group = group), fill = "blue", color = "black") +
  coord_quickmap()
p
#Dallas Coord
Dallas <- tibble(
  long = c(-96.7970),
  lat = c(32.7767),
  names = c("Dallas")
)
  
p + geom_point(data = Dallas, aes(x=long, y = lat), shape = 21, color = "black", fill = "yellow", size = 5) +
  geom_text(data = Dallas, aes(x = long, y = lat, label = names), hjust = 0, nudge_x = 1,color = "white")

#themes

mpg %>% ggplot(mapping = aes(x = class, fill = class)) + 
  geom_bar() +
  theme_excel()


p = mpg[mpg$class == "compact",] %>% 
  ggplot(aes(x = cty, fill = drv)) + 
  geom_histogram(color = "blue") 

ggplotly(p)

p <- plot_ly(mpg, x = ~cty, y = ~hwy, z = ~displ, color = ~drv) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'City MPG'),
                      yaxis = list(title = 'Displacement'),
                      zaxis = list(title = 'Highway MPG')))
p

p <- plot_ly(iris, x = ~Sepal.Width, y = ~Sepal.Length, z = ~Petal.Width, color = ~Species) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Sepal Width'),
                      yaxis = list(title = 'Sepal.Length'),
                      zaxis = list(title = 'Petal.Width')))
p

#Book Stuff
ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )


