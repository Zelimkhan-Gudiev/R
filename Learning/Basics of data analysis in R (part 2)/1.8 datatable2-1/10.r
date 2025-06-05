library(data.table)

purchases <- fread("/Users/zelimkhan/Desktop/Data/GitHub/DF/1.8 stepik/purchases.csv")
products <- fread("/Users/zelimkhan/Desktop/Data/GitHub/DF/1.8 stepik/products.csv")

purchases.with.brands <- merge(
  purchases,
  products[, list(product_id, brand)],
  by="product_id"
)

pop.20.brands <- head(
  purchases.with.brands[, 
                        list(
                          total.brand.users = length(unique(externalsessionid))
                        ),
                        by=brand][order(-total.brand.users)], 20)

users <- purchases.with.brands[, list(unique.brands = length(unique(brand)),
                           items = .N,
                           brand = brand[1]),
                    by=externalsessionid]

brand.loyal.users <- users[items > 1][unique.brands == 1][, list(total.loyal.users = .N), by=brand]

brand.stats <- merge(
  pop.20.brands,
  brand.loyal.users,
  by="brand"
)

brand.stats[, loyal := total.loyal.users / total.brand.users]

brand.stats[order(-loyal)]


# Step 7 of 9
# Создайте функцию get.category.ratings, которая будет возвращать суммарный оборот (с учетом скидок) каждой категории, и количество 
# купленных предметов по таблице покупок и таблице принадлежности товара к категории. Если купленный товар принадлежит нескольким категориям,
# его необходимо учитывать во всех. При решении используйте ключи.
# NB! В этой задаче нет столбца "price", но есть столбец "totalcents", содержащий стоимость всей позиции заказа
# (т.е. он уже умножен на quantity), и с учетом скидок.

product.category <- data.table(product_id = c(1,1,2,2,3),
                               category_id = c(1,2,1,3,3))
purchases <- data.table(product_id = c(1, 2, 3),
                        totalcents = c(100, 200, 300),
                        quantity = c(1, 1, 3))

get.category.ratings(purchases, product.category)
#          category_id    totalcents    quantity
#  1:           1            300           2
#  2:           2            100           1
#  3:           3            500           4

# 1
get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  df <- merge(purchases, 
              product.category[, list(product_id, category_id)])
  df[, .(totalcents = sum(totalcents), quantity = sum(quantity)), by =  category_id]
}
# 2
get.category.ratings <- function(purchases, product.category) {    
  setkey(purchases, product_id)    
  setkey(product.category, product_id)    
  purchases.product.category <- merge(purchases, product.category, by='product_id')    
  purchases.product.category[, list(totalcents = sum(totalcents), quantity = sum(quantity)), by=category_id]}
# 3
get.category.ratings <- function(purchases, product.category) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  merge(purchases, purchases, by = "product_id")[, 
                                                        .(totalcents = sum(totalcents), quantity = sum(quantity)), by = category_id]
}
# 4
get.category.ratings <- function(purchases, pc) {
  setkey(purchases, product_id)
  setkey(product.category, product_id)
  merge(purchases, product.category, all=T)[,.(totalcents=sum(totalcents), quantity=sum(quantity)), by=category_id]
}

# 5
get.category.ratings <- function(purchases, product.category) {
  merge(setkey(product.category, product_id), 
        setkey(purchases, product_id), 
        by = 'product_id')[, .(
          totalcents = sum(totalcents), 
          quantity = sum(quantity)
        ), 
        by = category_id]
}

# Step 8 of 9
# Напишите функцию, которая будет с помощью := добавлять столбец «price.portion», содержащий процент стоимости товара в заказе,
# с двумя знаками после запятой (нули после запятой не опускать). Проверяться будет возвращаемая из функции таблица. 
# Тип нового столбца - character (строка). Записи с неположительным количеством товаров убрать перед расчётом.
# Например: если в заказе есть три позиции: 1 ед. по 100р, 1 ед. по 300 руб. и 2 ед. по 50 р., в столбце должно быть «20.00, 60.00, 20.00:
sample.purchases <- data.table(price = c(100, 300, 50, 700, 30),
                               ordernumber = c(1,1,1,2,3),
                               quantity = c(1,1,2,1,-1),
                               product_id = 1:5)
mark.position.portion(sample.purchases)
#      price     ordernumber  quantity   product_id     price.portion
# 1:   100            1          1           1              20.00
# 2:   300            1          1           2              60.00
# 3:    50            1          2           3              20.00
# 4:   700            2          1           4              100.00

# Обратите внимание, что квадратные скобки с оператором := так же возвращают data.table, но обернутый в invisible.
# При решении функцией merge пользоваться запрещено, и порядок столбцов в таблице purchases не должен меняться.
# 1 
mark.position.portion <- function(purchases) {
  purchases <- purchases[quantity>=0, ] 
  purchases[ , 
             price.portion := format( round( price * quantity / sum(price * quantity ) * 100, 2), nsmall = 2),
             by = .(ordernumber)
  ][]
}
mark.position.portion(sample.purchases)

# 2
mark.position.portion <- function(purchases) {  
  purchases <- purchases[quantity > 0]  
  purchases[, price.portion := format(round(100 * price * quantity / sum(price * quantity), 2),                                       
                                      nsmall=2,digits=2, scientific = F), by=ordernumber]}

# 3
mark.position.portion <- function(purchases) {
  p <- purchases[quantity>0]
  p[, price.portion := ""][quantity>0, price.portion := sprintf("%.2f", quantity*price/sum(quantity*price) * 100), by = ordernumber]
  return(as.data.table(p))
}

# 4
mark.position.portion <- function(purchases) {
  kis <- purchases[quantity>0]
  kis1 <- kis[,price.portion:=sprintf("%.2f",((price*quantity)*100/sum(price*quantity))), by='ordernumber']
}
