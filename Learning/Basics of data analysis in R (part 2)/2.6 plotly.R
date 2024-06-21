library(data.table)
library(plotly)

purchases <- fread("C:/Users/GudievZK/Desktop/GitHub/DF/1.7 stepik/purchases.csv")

price.hist <- ggplot(purchases) + 
  geom_histogram(aes(totalcents), fill="white", color="black") +
  scale_x_log10("Item price", labels = function(x) {
    format(x / 100, scientific = F, big.mark = " ")
  }) +
  ylab("Times purchased")

interactive.price.hist <- ggplotly(price.hist)

interactive.price.hist


plot_ly(mtcars, 
        x = mpg, 
        y = ~ disp, 
        text = rownames(mtcars), 
        group = as.factor(mtcars$cyl), 
        mode="markers", 
        type="scatter")

plot_ly(mtcars, x = mpg, y = mtcars$disp, text = rownames(mtcars), group = as.factor(mtcars$cyl), mode="markers", type="scatter")

plot_ly(mtcars,
        x = mtcars$mpg,
        y = mtcars$disp,
        text = rownames(mtcars),
        group_by = as.factor(mtcars$cyl),
        mode="markers",
        type="scatter")


smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), 
                                economics, span = 0.2))
plot_ly(economics, 
        x = ~ date, 
        y = ~ uempmed, 
        type = "scatter",
        showlegend = FALSE, 
        mode="lines+markers") %>%
  add_trace(x = date, y = smoothed.vector)

smoothed.vector <- fitted(loess(uempmed ~ as.numeric(date), economics, span = 0.2))
my.plot <- ggplot(economics) + 
  geom_line(aes(x = date, y = uempmed), color = "red") + 
  geom_line(aes(x = date, y = smoothed.vector), color = "blue")
ggplotly(my.plot)


purchases[, log.tc := log(purchases$totalcents)]
purchases[, normal.approx.prob := dnorm(log.tc, mean(log.tc), sd(log.tc))]

ggplotly(ggplot(purchases, aes(log.tc)) + 
           geom_histogram(aes(y=..count.. / max(..count..)), fill="white", color="blue", binwidth = 0.2) + 
           geom_line(aes(log.tc, normal.approx.prob / max(normal.approx.prob)), color="red"))


plot_ly(z = ~volcano, type="surface")


mesh <- data.table(
  x = rnorm(40),
  y = rnorm(40),
  z = rnorm(40)
)
plot_ly(mesh, type="mesh3d", x = ~x, y = ~y, z = ~z, alphahull = 0)


points <- data.table(
  x = c(0.2, 0.8, 0, 1),
  y = c(0, 0, 1, 1),
  z = c(0, 0, 0, 0)
)
i.s <- c(0, 2)
j.s <- c(1, 1)
k.s <- c(2, 3)
plot_ly(points, x = ~x, y = ~y, z = ~z, i = ~i.s, j = ~j.s, k = ~k.s, type = "mesh3d")
