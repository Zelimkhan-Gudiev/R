library(readxl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)
library(rlang)

# Загрузка данных
df <- read_excel("C:/Users/GudievZK/Desktop/GitHub/AC/DA-21 Идентифицировать и проанализировать закупки ОИВ для собственных нужд/2024.06.19_DA-21__MKR_Отчет по закупкам за 2023.xlsx")

# Переименовывание колонок
colnames(df) <- c("kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date",
                  "using_tz", "notice_publication_date", "subject_of_procurement", "contract_date", "lot_id",
                  "contract_eis_registry_number", "contract_completion_date", "law", "method_of_determinig_supplier",
                  "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
                  "sum_of_contracts")

# Заполнение пустых ячеек
df <- fill(df, "kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date")

# Удаление лишних столбцов и строк
df <- df[-c(1, 2),]
df <- filter(df, str_detect(df$kod_kpgz, "^01.15")) # удаляем позиции КПГЗ, не начинающиеся с 01.15

# Считаем количество закупок, у которых нет рег. номера контракта/договрова
df %>% filter(contract_eis_registry_number == "-") %>% distinct(lot_id, grbs, customer, supplier, name_tz, tz_approval_date,
                                                                using_tz, notice_publication_date, subject_of_procurement, 
                                                                contract_date, lot_id, contract_eis_registry_number, contract_completion_date, 
                                                                law, method_of_determinig_supplier, 
                                                                Basis_for_concluding_a_contract_with_a_single_supplier, 
                                                                object_status, number_of_contracts,
                                                                contract_eis_registry_number, .keep_all = T)

# Считаем количество закупок, у которых есть рег. номер контракта/договрова
df %>% filter(!contract_eis_registry_number == "-") %>% distinct(lot_id, contract_eis_registry_number, .keep_all = T) %>% nrow()


df <- filter(df, !str_detect(df$customer, "^ГБ|^\"ГБ|^ГК|^АО|^ГАУ|^ГУП|^ГАПОУ|^ГАОУ|^ООО|^«НИИ|^МАОУ|
                                  |^СС и НМП|^ОАО|^«Мой особый семейный|^КП|^БЮРО|^ГПБУ|^МБУ|^МАДОУ|
                                  |^МАУ|^МУ |^МУК |^МОСКОВСКИЙ ГОСУДАРСТВЕННЫЙ ОБЪЕДИНЕННЫЙ МУЗЕЙ-ЗАПОВЕДНИК|
                                  |^Фонд реновации|^МГУУ Правительства Москвы, Университет Правительства Москвы|
                                  |^Московский фонд защиты прав дольщиков|^ЦПКиО им. М. Горького|
                                  |^«Московский театр «Современник»"))
      



df1 <- df_temp %>% filter(str_detect(df_temp$subject_of_procurement, "для нужд"))
