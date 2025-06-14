seq_x <- round(seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 5))

ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) + 
  geom_point() +
  
  scale_x_continuous(name = "Miles/(US) gallon", 
                     breaks = c(1, seq(10, 35, 5)), 
                     limits = c(1, 35)) +
  scale_y_continuous(limits = c(50, 400)) +
  scale_color_manual(values = c("Blue", "Black"), 
                     name = "Legend name", 
                     labels = c("Auto", "Manual"))


ggplot(mtcars, aes(hp, fill = factor(am)))+
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("Red", "Green"))
  

ggplot(mtcars, aes(hp, mpg, size = disp, shape = factor(vs))) + 
  geom_point() +
  scale_size_continuous(name = "Any name", 
                        breaks = seq(100, 400, 40)) +
  scale_shape_discrete(name = "Any name")


ggplot(mtcars, aes(factor(am), hp)) +
  geom_boxplot() +
  scale_x_discrete()

ggplot(mtcars, aes(factor(am), hp, fill = factor(cyl))) + 
    geom_boxplot() +
    scale_fill_brewer(type = "qual", palette = 3) +
    theme_bw()

ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) + 
    geom_point(size = 5) +
    scale_color_brewer(type = "qual", palette = 6) +
    theme_dark()


install.packages("ggthemes")
library("ggthemes")

ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) + 
    geom_point(size = 2) +
    theme()


### Step 6 of 10
ggplot(iris, aes(Sepal.Length, 
                 Petal.Length, 
                 color = factor(Species), 
                 group = factor(Species)
                 )
       ) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_color_discrete(name = "Вид цветка",
                       labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский")) +
  scale_x_continuous(name = "Длина чашелистика",
                     breaks = seq(4, 8, 1),
                     limits = c(4, 8)
                     ) +
  scale_y_continuous(name = "Длина лепестка",
                     breaks = seq(1, 7, 1),
                     limits = c(1, 7))
  








