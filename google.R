remove(list = ls())

# install
# install.packages("devtools")
devtools::install_github("tidyverse/googlesheets4")

# lib
library(googlesheets4)

# авторизация

gs4_auth(email = 'GudievZK@gmail.com')

# данные для теста
my_iris   <- iris
my_mtcars <- mtcars

# создаём докс
ss <- gs4_create("demo_dox", 
                    sheets = list(iris   = head(my_iris), 
                                  mtcars = my_mtcars))
# открыть созданный Google Dox
gs4_browse(ss)

# создать новый лист
sheet_add(ss, 
          sheet = "mtcars_new", 
          .after = "mtcars")

# запись данных на новый лист
sheet_write(data = my_iris,
             ss = ss, 
             sheet = "iris_new")

# дописать значиения
sheet_append(data  = tail(my_iris, 20),
              ss    = ss, 
              sheet = "iris")

# получить список листок google таблицы
sheet_names(ss)

# чтение листа из гугл таблиц
dolya <- as_sheets_id("1ltR2wYrmN1tPla6TJfvpvG-IwInB_43Gth_4Ywws5S8")

dolya_s <- read_sheet(dolya, 
                    sheet = "Export Worksheet")
names(dolya)

dolya <- range_read(dolya, sheet = "Export Worksheet")
delAndSt <- subset(dolya, IS_STANDARD_PRODUCT == 2)
detAndSt <- subset(dolya, IS_STANDARD_PRODUCT == 3)

condStand <- subset(dolya, IS_STANDARD_PRODUCT %in% c(2, 4, 5, 51, 6))
