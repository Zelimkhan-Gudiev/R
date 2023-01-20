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
  
}
# Experiment
purchases[order(price, decreasing = T) & quantity >= 0, list(ordernumber, product_id)]
sample.purchases[order(price, decreasing = T) & quantity >= 0, list(ordernumber, product_id)]
sample.purchases[quantity >= 0 & order(price, decreasing = T), ]

# Проверка

sample.purchases <- data.table(price = c(100000, 6000, 7000, 5000000),
                               ordernumber = 1:4,
                               quantity = c(1,2,1,-1),
                               product_id = 1:4)

ordered.short.purchase.data(sample.purchases)



# step 11 of 11
