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
filter.expensive.available <- function(products, brands) {
  
}
# step 10 of 11


# step 11 of 11
