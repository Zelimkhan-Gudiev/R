library(ggplot2)

ggplot(diamonds, aes(x = price)) +
  geom_histogram()

ggplot(diamonds, aes(x = price, 
                     y = carat)) +
  geom_point(size = 0.5, alpha = .2)+
  geom_smooth(size = 1, color = "red")

str(airquality)

library(dplyr)

glimpse(airquality)

gr_airquality <- group_by(airquality, Month)

t <- summarise(gr_airquality, 
               mean_temp = mean(Temp), 
               mean_wind = mean(Wind))

ggplot(t, aes(Month, mean_temp))+
  geom_line() +
  geom_point(aes(size = mean_wind), color = "red") +
  geom_hline(yintercept = 75, linetype = "dotted", 
             size = 1.2, color = "blue")
?geom_errorbar()
?geom_pointrange()

gr_mtcars <- group_by(mtcars, am, cyl)

se_data <- summarise(gr_mtcars, 
                     mean_mpg = mean(mpg), 
                     y_max = mean(mpg) + 
                         1.96 * sd(mpg) / sqrt(length(mpg)), 
                     y_min = mean(mpg) - 
                         1.96 * sd(mpg) / sqrt(length(mpg)))

ggplot(se_data, aes(x = factor(am), y = mean_mpg, 
                    col = factor(cyl), 
                    group = factor(cyl))) +
    geom_line()+
    geom_errorbar(aes(ymin = y_min, ymax = y_max), width = 0.2) +
    geom_point(shape = 21, size = 3, fill = "white")


ggplot(se_data, aes(x = factor(am), y = mean_mpg, 
                    col = factor(cyl), 
                    group = factor(cyl))) +
    geom_pointrange(aes(ymin = y_min, ymax = y_max)) +
    geom_line()

ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), 
                   group = factor(cyl))) + 
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
                 width = 0.2) + 
    stat_summary(fun.data = mean_cl_boot, geom = "point", size = 2) +
    stat_summary(fun.y = mean, geom = "line")

sd_error <- function(x){
    c(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
}


ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), 
                   group = factor(cyl))) + 
    stat_summary(fun.data = sd_error, geom = "errorbar", 
                 width = 0.2, 
                 position = position_dodge(0.2)) + 
    stat_summary(fun.data = sd_error, geom = "point", size = 2, 
                 position = position_dodge(0.2)) +
    stat_summary(fun.y = mean, geom = "line", 
                 position = position_dodge(0.2))

# Step 7 of 11

my_plot <- ggplot(mtcars, aes(factor(am), mpg)) +
                  geom_violin() +
                  geom_boxplot(width = 0.2)

# Step 8 of 11
sales <- read.csv("/Users/zelimkhan/Desktop/Data/GitHub/DF/2.2 sales.csv") %>% as_tibble()

# Step 9 of 11
my_plot <- ggplot(sales, aes(income, sale)) + 
            geom_point(aes(col = shop)) + 
            geom_smooth()
            
# 2

my_plot <- ggplot(sales, aes(income, sale)) + 
                  geom_point(aes(color = shop)) + 
                  geom_smooth(method = "lm")

# 3
my_plot <- ggplot(sales, aes(income, sale)) + 
                  geom_point(aes(color = shop)) + 
                  geom_smooth(color = "blue")

# Step 10 of 11

my_plot <- ggplot(sales, aes(x = shop, 
                             y = income, 
                             color = season,
                             group = season)
                            ) +
                  stat_summary(fun.data = mean_cl_boot, 
                               geom = "pointrange",
                               position = position_dodge(0.2)
                               )
  
# 2
my_plot <- ggplot(sales, aes(x = shop, 
                             y = income, 
                             color = factor(season))) +
                  stat_summary(fun.data = mean_cl_boot, 
                               position = position_dodge(0.2))
# Step 11 of 11
my_plot <-  ggplot(sales, aes(x = date,
                              y = sale,
                              color = shop)) +
                  stat_summary(fun.data = mean_cl_boot, 
                               geom = "errorbar",
                               position = position_dodge(0.2)) + # добавим стандартную ошибку
                  stat_summary(fun.data = mean_cl_boot, 
                               geom = "point",
                               position = position_dodge(0.2)) + # добавим точки
                  stat_summary(fun.data = mean_cl_boot,
                               geom = "line",
                               position = position_dodge(0.2)) # соединим линиями
