# step 2 mutate_each
library(ggplot2)
library(dplyr)

d <- as_data_frame(matrix(rnorm(30), ncol = 5))

mutate_each(d, funs(ifelse(. < 0, 0, .)))
mutate_each(d, funs(ifelse(. < 0, 0, .)))

col_1 <- d$V1
col_2 <- d$V2

ifelse(col_1 < 0, 0, col_1)
ifelse(col_2 < 0, 0, col_2)

my_fun <- function(x) ifelse(x < 0, 0, x)
sapply(d, function(z) abs(z))

# Step 3 of 16
# Напишите функцию, all_to_factor, которая преобразует dataframe, переводя все его переменные в фактор.
all_to_factor <- function(x){
  x_factor <- mutate_each(x, funs(as.factor(.)))
  return(x_factor)
}
#
str(mtcars)
m <- mutate_each(mtcars, funs(as.factor(.)))
str(m)

# 2
all_to_factor <- function(x){
  mutate_each(x, funs(as.factor))
}

# 3
all_to_factor <- function(x){
  mutate_all(x, factor)
}


# Step 4 of 16
# В этом задании от вас потребуется написать функцию для предобработки данных log_transform. В статистике часто трансформируют исходные переменные.
# Например, используют значение натурального логарифма исходной переменной.
# Ваша задача написать функцию, которая получает на вход dataframe  с произвольным числом переменных разных типов. 
# На первом этапе функция должна выполнить предобработку числовых переменных. Т.к. значение логарифма мы можем рассчитать только для положительных чисел. 
# Для этого сделаем центрирование всех переменных (Rescaling), только еще добавим единичку, чтобы у нас не осталось нулей: ...
# После того как мы масштабировали каждую переменную, осталось рассчитать значение натурального логарифма каждого наблюдения (функция log) 
# и вернуть новый dataframe. 
log_transform <- function(test_data){
  neg_num_test_data <- select_if(test_data, Negate(is.numeric))
  num_test_data <- select_if(test_data, is.numeric)
  resc_test_data <- mutate_each(num_test_data, funs((. - range(.)[1]) / (range(.)[2] - range(.)[1]) + 1))
  log_resc_test_data <- log(resc_test_data)
  finish_data <- cbind(log_resc_test_data, neg_num_test_data)
}

# 2
log_transform <- function(test_data){      
  rescaling <- function(x){      
    log((x - min(x)) / (max(x) - min(x)) + 1)
    }      
  num_var <- sapply(test_data, is.numeric)      
  test_data[num_var] <- mutate_each(test_data[num_var], funs(rescaling))      
  return(test_data)
}
# 3
log_transform <- function(test_data){
  test_data %>% mutate_if(is.numeric, funs(log(scales::rescale(.) + 1)))
}



# step 6 group_by 
diamonds <- as_data_frame(diamonds)

gr_diamonds <-  group_by(diamonds, cut)

sample_n(diamonds, 2)
slice(diamonds, 1)

sample_n(gr_diamonds, 2)

slice(gr_diamonds, 1)


# step 7 group_by and summarise
?summarise()

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y))

# step 8
gr_diamonds <-  group_by(diamonds, cut, color)
summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y))

summarise(gr_diamonds,
          numbers = n(),
          mean_price = mean(price), 
          mean_x = mean(x), 
          median_y = median(y), 
          min_y = min(y), 
          great_price = sum(price > 5000))


# step 9
gr_mtcars <- group_by(mtcars, am, vs)
my_means <- summarise_all(gr_mtcars, funs(mean))

# Step 10 of 16
# В разобранном примере мы сами указали, какие переменные мы использовать внутри функции summarise, однако, часто может возникнуть 
# необходимость обратиться сразу к большому числу переменных в данных, разумеется нет необходимости прописывать названия каждой 
# переменной в ручную. На помощь придет функция summarise_each.
# Рассмотрим пример, допустим мы хотим рассчитать среднее и стандартное отклонение для каждой группы в данных Iris:
group_by(iris, Species) %>% 
  summarise_all(funs(sd, mean))


# Step 11 of 16

select(mtcars, "am", hp)
mtcars[, am]
mtcars[, "am"]
mtcars[["am"]]
mtcars["am"]

var_to_select <- "hp"
select_(mtcars, var_to_select)
select_(mtcars, 'hp')



mtcars$am <-factor(mtcars$am)
mtcars$vs <-factor(mtcars$vs)


factor_vars <- names(which(sapply(mtcars, is.factor))) # сохраним имена факторов в вектор
mtcars %>% 
  group_by_(.dots = factor_vars) %>% 
  summarise(n = n())

mutate(mtcars, new_var = (hp - mean(hp)) / sd(hp))
mutate(mtcars, new_var = (hp - mean(hp)) / sd(hp))

mini_mtcars <- select(mtcars, hp, am, vs)
mini_mtcars <- mini_mtcars %>% mutate(am = factor(am), 
                                      vs = factor(vs))

mutate_(mini_mtcars, new_var = "(hp - mean(hp)) / sd(hp)")
mutate_(mini_mtcars, new_var = ~ (hp - mean(hp)) / sd(hp))
mutate_(mini_mtcars, new_var =  quote((hp - mean(hp)) / sd(hp)))

library(lazyeval)
num_var <- names(which(sapply(mini_mtcars, is.numeric)))
mutate_(mini_mtcars, new_var = interp(~(var - mean(var)) / sd(var), var = as.name(num_var)))


library(lazyeval)
var_for_group <- c("am", "vs")
var_for_filter <- "hp"
var_for_arrange <- "mpg"
var_for_mutate <- "qsec"
var_for_summirise <- "cyl"
group_by_(mtcars, .dots = var_for_group) %>% 
  filter_(interp(~var > 100, var = as.name(var_for_filter))) %>% 
  arrange_(var_for_arrange) %>% 
  mutate_(new_var = interp(~ifelse(var > mean(var), 1, 0), 
                           var = as.name(var_for_mutate))) %>% 
  summarise_(max = interp(~max(var), var = as.name(var_for_summirise)))

# Step 13 of 16
dataset <- read.csv("https://stepic.org/media/attachments/course/724/salary.csv", stringsAsFactors = T)
str(test_data)
# dataset[sapply(dataset, is.character)] <- lapply(dataset[sapply(dataset, is.character)], as.factor)
dataset_num <- select_if(dataset, is.numeric)
factor_vars <- names(which(sapply(test_data, is.factor)))
  
descriptive_stats <- function(dataset) {
df <- dataset %>%                    
            group_by(gender, country) %>% 
            summarise(n = n(),
                      mean = mean(salary, na.rm = T),
                      sd = sd(salary, na.rm = T),
                      median = median(salary, na.rm = T),
                      first_quartile = quantile(salary, 0.25, na.rm = T),
                      third_quartile = quantile(salary, 0.75, na.rm = T),
                      na_values = sum(is.na(salary))
)
return(df)
}
descriptive_stats(dataset)

# 2
descriptive_stats <- function(test_data){      
  gr_data <- group_by(test_data, gender, country)      
  result <- summarise(gr_data,                       
                      n = n(),                      
                      mean = mean(salary, na.rm = T),                       
                      sd = sd(salary, na.rm = T),                      
                      median = median(salary, na.rm = T),                       
                      first_quartile = quantile(salary, na.rm = T)[2],                       
                      third_quartile = quantile(salary, na.rm = T)[4],                       
                      na_values = sum(is.na(salary)))
}

# 3
descriptive_stats <- function(df){
  df %>% 
    group_by_(.dots = names(which(sapply(df, is.factor)))) %>%
    summarise_if(is.numeric, funs(n=n(), 
                                  mean = mean(., na.rm=T),
                                  sd = sd(., na.rm=T),
                                  median = median(., na.rm=T),
                                  first_quartile = quantile(., probs=0.25, na.rm=T),
                                  third_quartile = quantile(., probs=0.75, na.rm=T),
                                  na_values = sum(is.na(.))
    ))
}






descriptive_stats <- function (dataset){
  df <- dataset %>% 
            group_by(gender, country) %>% 
            summarise(funs(n = n(salary),
                      mean = mean(salary, na.rm = T),
                      sd = sd(salary, na.rm = T),
                      median = median(salary, na.rm = T),
                      first_quartile = quantile(salary, 0.25, na.rm = T),
                      third_quartile = quantile(salary, 0.75, na.rm = T),
                      na_values = sum(is.na(salary))))
  return(df)
}

# Step 15 of 16
test_data <- mtcars

to_factors <- function(test_data, factors){
  factors_names <- names(test_data[factors])
  test_data1 <- test_data %>% 
                    mutate_at(factors_names, funs(ifelse(. > mean(., na.rm = T), 1, 0)))
                    test_data1[factors_names] <- lapply(test_data1[factors_names], factor)
    return(test_data1)
}

to_factors(mtcars, c(1, 3))

# 2
to_factors <- function(test_data, factors){    
  test_data[factors] <- mutate_each(test_data[factors], funs(factor(ifelse(. > mean(.), 1, 0))))    
  return(test_data)
}

# 3
to_factors <- function(test_data, factors){
  mutate_at(test_data, factors, funs(factor(. > mean(.), labels = 0:1)))
}

# 4
to_factors <- function(test_data, factors){
  test_data <- mutate_each(test_data, funs(as.factor(ifelse(. > mean(.), 1, 0))), factors)
  return(test_data)
}

# Step 16 of 16

high_price <- diamonds %>% 
              group_by(color) %>% 
              select(color, price) %>% 
              arrange(desc(price)) %>% 
              slice(1:10)


