data <- data.frame(x1 = 1:5,                         # Create example data frame
                   x2 = LETTERS[1:5],
                   x3 = 2,
                   x4 = factor(c(1, 3, 2, 2, 1)),
                   stringsAsFactors = FALSE)

str(data)
typeof(data$x1)
class(data$x1)
typeof(data$x2)
class(data$x2)
typeof(data$x3)
class(data$x3)

num_cols <- unlist(lapply(data, is.numeric))
data_num <- data[ , num_cols]

library(dplyr)

data_num2 <- select_if(data, is.numeric)
pairs(data_num2)

yt
ytNum <- select_if(yt, is.numeric)
pairs(ytNum)


names(ytNum)
dontNeed <- yt[, -c("numb", "year_plan_st", "kvartal", "created_date", "time_plan", "date_end")]
dontNeed <- yt[, -c(yt$numb, yt$year_plan_st, yt$kvartal, yt$created_date, yt$time_plan, yt$date_end)]
ytNum <- 