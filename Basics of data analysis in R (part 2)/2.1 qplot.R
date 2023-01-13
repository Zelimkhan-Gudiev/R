library(ggplot2)

data("diamonds")

qplot(x = price, data = diamonds)
qplot(x = price, y = carat, data = diamonds)
qplot(x = cut, y = carat, data = diamonds)

v <- diamonds$carat
qplot(v)
qplot(diamonds$carat)

# Step 6 of 14
depth_hist <- qplot(diamonds$depth)

qplot(diamonds$carat, diamonds$price)

my_plot <- qplot(x = price, y = carat, data = diamonds)

qplot(x = price, 
      y = carat,
      color = color,
      shape = cut,
      data = diamonds, 
      geom = c("point", "smooth"))

qplot(mpg, 
      hp,
      color = I("blue"),
      shape = factor(cyl),
      size = I(5),
      alpha = I(0.3),
      data = mtcars)

qplot(x = price, data = diamonds, 
      fill = I("white"), 
      col = I("black"))

qplot(x = price,
      fill = color,
      data = diamonds,
      col = I("black"))

qplot(x = price,
      fill = color,
      alpha = I(0.2),
      data = diamonds,
      col = I("black"), 
      geom = "density")

# Step 9 of 14
price_carat_clarity_points <- qplot(x = carat,
                                    y = price,
                                    data = diamonds,
                                    color = clarity)

# Step 11 of 14
x_density <- qplot(x = x,
                   data = diamonds,
                   geom = 'density')


# Step 12 of 14

x_cut_density <- qplot(x = x,
                   data = diamonds,
                   color = cut,
                   geom = 'density')

# Step 13 of 14
price_violin <- qplot(x = color,
                      y = price,
                      data = diamonds,
                      geom = "violin")

