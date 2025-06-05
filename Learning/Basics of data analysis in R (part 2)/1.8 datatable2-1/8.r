library(data.table)


products <- fread("/Users/zelimkhan/Desktop/Data/GitHub/DF/1.8 stepik/products.csv", colClasses = c(price = "double"))


products[price < 1000,
         name.with.price := paste0(name, " ( ", price, " руб.)")]
products[order(-price)]


products[, price := price / max(price), by=brand]

products

products[
  order(-price), 
  .(name = head(name, 3),
    price = head(price, 3)),
  by = brand
]

products[
  order(-price),
  head(.SD,3),
  by = brand
]
