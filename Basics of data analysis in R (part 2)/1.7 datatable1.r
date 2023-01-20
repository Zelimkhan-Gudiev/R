# step 1 fread function

library(data.table)

system.time(fread("C:/Users/GudievZK/Desktop/GitHub/DF/1.7_Stepic_DataTable/products.csv"))
system.time(read.table("C:/Users/GudievZK/Desktop/GitHub/DF/1.7_Stepic_DataTable/products.csv", header = T, sep = ";"))

products <- fread("C:/Users/GudievZK/Desktop/GitHub/DF/1.7_Stepic_DataTable/products.csv", encoding = "UTF-8")


# step 2 data.table vs dataframe
products[1:10, ]
products[products$price > 10000, ]

with(iris, iris[Species == "virginica", ])
#iris$Species
products[price > 10000]


products[(price > 1000) & 
           (brand %in% c("Epson", "Apple"))]


# step 3 data filtering

products[available, ]
products[available == TRUE, ]

products[3, ]
iris[3, ]
products[3]
iris[3]

products[!(brand %in% c("Apple", "Epson"))]

products[!(1:10)]

# step 4 data transformation

products[, list(name,
                price.1k = price / 1000)]

order(products$price, decreasing = T)
products[order(price, decreasing = T)]
products[order(price, decreasing = T), list(name, price.1k = price / 1000)]

products[order(price, decreasing = T),
         list(name, price.1k = paste0(price / 1000, " тыс.руб"))]

head(products[order(price, decreasing = T), 
              list(name, price.1k = paste0(price / 1000, " тыс.руб"))], 5)

# step 5 data transformation advanced


products[order(price, decreasing = T),
         list(price.1k = paste0(price / 1000, " тыс.руб"))]$price.1k

products[, list(name, price)]
products[, .(name, price)]
products[, c("name", "price"), with = F]

products[order(-price), .(name = head(name), 
                          price = head(price))]


products[, .(price = sum(price))]


a <- products[, list(name.with.brand = paste0(brand, " - ", name))]
a[order(name.with.brand)]

products[, list(name.with.brand = paste0(brand, " - ", name))][order(name.with.brand)]


products[, .(price = {
  a <- mean(price)
  b <- median(price)
  c(min(price), max(price), a/b)
})]

products[, .(mean.price = mean(price)), by = brand]
products[order(-price), .(name = head(name, 3),
                          price = head(price, 3)), by = brand]
levels(as.factor(products$brand))
unique(products$brand) %>% length()

# step 9 of 11
products$brand %>% unique() %>% table() %>% sum()

brands <- c("Hiltt", "Kipor")
products[(price >= 5000000 & available == T & brand %in% brands)]

filter.expensive.available <- function(products, brands) {
  products[(price >= 500000 & available == T & brand %in% brands)]
}

sample.products <- data.table(price = c(10000, 600000, 700000, 1000000),
                              brand = c("a", "b", "c", "d"),
                              available = c(T, T, F, T))

filter.expensive.available(sample.products, c("a", "c", "d"))

# 2
filter.expensive.available <- function(products, brands) {    
  products[brand %in% brands][price >= 500000][available == T]}

# 3
filter.expensive.available <- function(products, brands) {
  r <- products[brand %in% brands & available==TRUE & price>=5000*100]
  r
}

# 4
filter.expensive.available <- function(products, brands) {
  products[(price > 500000) & (available == T) & (brand %in% brands)]
}

# step 10 of 11
# Создайте функцию ordered.short.purchase.data, которая будет принимать purchases, объект data.table, и возвращать таблицу 
# только со столбцами с номером заказа и ID продукта.
# Упорядочите результат по убыванию стоимости купленного товара. Возвраты (записи с отрицательным количеством предметов в позиции) надо удалить.
purchases <- fread("C:/Users/GudievZK/Desktop/GitHub/DF/1.7_Stepic_DataTable/purchases.csv", encoding = "UTF-8")

ordered.short.purchase.data <- function(purchases) {
  purchases[quantity >= 0, ][order(price, decreasing = T), list(ordernumber, product_id)]  
}

# Проверка
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)
ordered.short.purchase.data(sample.purchases)

# 2
ordered.short.purchase.data<- function(purchases) {    
  purchases[order(-price)][quantity >= 0][, .(ordernumber, product_id)]
}

# 3 
ordered.short.purchase.data <- function(purchases) {
  purchases[quantity >= 0][order(-price), .(ordernumber, product_id)]
}

# 4
ordered.short.purchase.data <- function(purchases) {
  first <- purchases[order(-price)]
  second <- first[!(quantity < 0), .(ordernumber, product_id)]
  second
}

# 5
ordered.short.purchase.data <- function(purchases){
  purchases1 <- purchases[!purchases$quantity<0]
  purchases2 <- purchases1[order(purchases1$price, decreasing = T)]
  purchases3 <- purchases2[, c("ordernumber", "product_id")]
  return(purchases3)
}


# step 11 of 11
# Напишите функцию purchases.median.order.price, у которой один аргумент: purchases, и которая возвращает медианную стоимость заказа (число).
# Группировку стоит проводить с помощью data.table. Записи с неположительным количеством купленных товаров (возвраты) игнорировать.
# Обратите внимание, что одному заказу может соответствовать несколько записей – «позиций» с одинаковым ordernumber, и что при расчете 
# стоимости заказа надо учитывать ситуации, когда пользователь купил несколько товаров одного типа (их количество указано в quantity).

purchases.median.order.price <- function(purchases) {
  s <- purchases[quantity >= 0, .(sumOfOrder = sum(price * quantity)),  by = ordernumber]
  median(s$sumOfOrder)
}

# Cheking
sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = c(1,2,2,3),
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

purchases <- sample.purchases
purchases.median.order.price(sample.purchases)
# 59500 median(c(100000, (6000 * 2 + 7000)))


# 2
purchases.median.order.price <- function(purchases) {    
  median(purchases[quantity >= 0][, list(w = sum(price * quantity)), by=list(ordernumber)]$w)
}

# 3

purchases.median.order.price <- function(purchases) {
  purchases %>% 
    filter(quantity > 0) %>% 
    group_by(ordernumber) %>% 
    summarise(order_total = sum(price * quantity)) %>% 
    summarise(median(order_total)) %>% .[[1]]
}

# 4
purchases.median.order.price <- function(purchases) {
  purchases[quantity > 0, .(value = sum(price * quantity)), by=ordernumber][, median(value)]
}

# 5
purchases.median.order.price <- function(purchases) {
  purchases[quantity >= 0, .(price = sum(price*quantity)), 
            by = ordernumber][,.(med = median(price))] %>%
    as.numeric()
}


