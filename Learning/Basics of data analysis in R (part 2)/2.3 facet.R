library(ggplot2)
library(dplyr)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ color)

mtcars <- mutate(mtcars, 
                 am = factor(am, labels = c("A", "M")), 
                 vs = factor(vs, labels = c("V", "S")))
                 
ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(col = factor(cyl))) + 
  facet_grid(vs ~ am) + 
  geom_smooth(method = "lm")


ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut, ncol = 1)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ .)

ggplot(diamonds, aes(carat, price))+
  geom_smooth() +   
  facet_grid(color ~ .)
  
# Step 7 of 10
mpg_facet <- ggplot(mtcars, aes(mpg)) +
  geom_dotplot() +
  facet_grid(am ~ vs)

# Step 8 of 10
sl_wrap <- ggplot(iris, aes(Sepal.Length)) + 
  geom_density() +
  facet_wrap(~ Species)


# Step 9 of 10
my_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ Species)

# Step 10 of 10
myMovieData <- read.table("C:/Users/GudievZK/Desktop/GitHub/DF/myMovieData.csv", header = T, sep = ",") %>% as_tibble()
myMovieData[, c(3, 4)] <- lapply(myMovieData[, c(3, 4)], factor)
myMovieData[, c(3, 4)] <- lapply(select(myMovieData, Year, Type), factor)
glimpse(myMovieData)

my_plot <- ggplot(myMovieData, aes(Type, Budget)) +
  geom_boxplot() +
  facet_grid(. ~ Year) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  





