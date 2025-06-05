library(readxl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)
library(rlang)
library(writexl)

spgz <- read_excel("C:/Users/GudievZK/Desktop/GitHub/AC/NT-44 Добавить в НТ исключения/2024.06.24 _NT-44_Каталог_СПГЗ с характ. с у.о. равно.xlsx", skip = 3)
spgz <- spgz[-1,]

colnames(spgz)
spgz <-fill(spgz, "Идентификатор СПГЗ", "Наименование СПГЗ", "КПГЗ", "ОКПД-2", "Стандартизирована", "КТРУ", 
                  "Единицы измерения", "Пакет", "Статус", "Актуальна", "Дата последнего изменения", "Удалена", 
                  "Есть ПЦП")

spgz <- spgz %>% filter(spgz$`Значение характеристики` == "Необходимо указать значение характеристики или удалить данный текст") %>% 
          distinct(`Идентификатор СПГЗ`,.keep_all = T)


spgz <- filter(spgz, spgz$`Наименование характеристики` %in% c("Функциональные характеристики", "Технические характеристики",
                                                 "Качественные характеристики", "Эксплуатационные характеристики") & 
        spgz$`Значение характеристики` == "Необходимо указать значение характеристики или удалить данный текст") %>% 
        distinct(`Идентификатор СПГЗ`,.keep_all = T)

table(as.factor(spgz$Стандартизирована))

write_xlsx(spgz,"C:/Users/GudievZK/Desktop/GitHub/AC/NT-44 Добавить в НТ исключения/2024.06.24 _NT-44_spgz.xlsx")

