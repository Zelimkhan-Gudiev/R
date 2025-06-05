 
library(readxl)
library(writexl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)

df <- read_xlsx("C:/Users/GudievZK/Downloads/Реестр CПГЗ_02_07_2024.xlsx", skip = 3)
df <- df[-1,]

colnames(df)
paste0(colnames(df), "'", ",", "'", collapse = "")

df <- fill(df,'Идентификатор СПГЗ','Наименование СПГЗ','КПГЗ','ОКПД-2',
     'Стандартизирована','КТРУ','Единицы измерения','Пакет','Статус',
     'Актуальна','Дата последнего изменения','Удалена','Есть ПЦП')

df <- df %>% 
  filter(`Статус` == "Утверждена" & `Актуальна` == "Да" & `Удалена` == "Нет")

df <- df %>% 
  filter(`Тип характеристики` == "Обязательная" & `Код характеристики КТРУ` == "-") %>% distinct(`Идентификатор СПГЗ`, .keep_all = T)
