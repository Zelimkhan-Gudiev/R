library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

getwd()
list.files()

#### Read list of tz
# tz <- read_excel("C:/Users/GudievZK/Desktop/SpinData/2023.12.06 Список шаблонов ТЗ.xls")
tz <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/Purchase/2023.12.06 tz.xlsx")
# tz <- fill(tz, "Наименование", "Статус", "Нетиповое ТЗ", "Обязательность использования", "Дата последнего изменения", "Дата утверждения")
# write_xlsx(tz, "C:/Users/GudievZK/Desktop/SpinData/2023.12.06 tz.xls")

tz <- separate(tz, КПГЗ, into = c("Код КПГЗ", "Наименование КПГЗ"), sep = "^\\S*\\K\\s+")

#### Read list of relations
# relations <- read_excel("C:/Users/GudievZK/Desktop/SpinData/2023.12.06 Панель мониторинга связей.xlsx")

relations <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/Purchase/2023.12.06 Панель мониторинга связеи??.xlsx")

relations <- separate(relations, "Вышестоящий КПГЗ (связанной позиции)", 
                      into = c("Код вышестоящей КПГЗ (связанной позиции)", 
                                                                   "Наименование вышестоящей КПГЗ (связанной позиции)"),
                      sep = "^\\S*\\K\\s+")


#### join dataframes
relations <- left_join(relations, tz, by = c("Код вышестоящей КПГЗ (связанной позиции)" = "Код КПГЗ"))


# write_xlsx(relations, "C:/Users/GudievZK/Desktop/SpinData/2023.12.06 relations.xlsx")

write_xlsx(relations, "/Users/zelimkhan/Desktop/Data/GitHub/Purchase/relations.xlsx")

relations %>% 
  filter(!is.na(Наименование))

relations_eq <- relations %>% 
  filter(`Тип связи` == "=", !is.na(Наименование), 
         `Нетиповое ТЗ` == "Нет")


relations_eq <- relations_eq %>% 
  select("Идентификатор в ПС «Каталог»...1", 
         "Идентификатор версии в ПС «Каталог»...2",
         "Идентификатор в ЕАИСТ...3",
         "Идентификатор версии в ЕАИСТ...4",
         "Единицы измерения...12",
         "Дата удаления позиции...22",
         "Код и наименование удаляемой КПГЗ/Наименование удаляемой СПГЗ"
         
         "Тип связи",
         
         "Идентификатор в ПС «Каталог»...30",
         "Идентификатор версии в ПС «Каталог»...31",
         "Идентификатор в ЕАИСТ...32",
         "Идентификатор версии в ЕАИСТ...33",
         "Код и наименование/Наименование связанной КПГЗ/СПГЗ",
         "Единицы измерения...41",
         "Наличие ПЦП или тарифа...39",
         "Текущий статус...42",
         "Дата создания позиции...49",
         "Дата удаления позиции...51",
         "Наименование",
         "Дата утверждения")
  
names(relations_eq)













