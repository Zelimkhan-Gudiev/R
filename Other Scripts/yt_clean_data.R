library(readxl)
library(dplyr)
install.packages("googlesheets4")
library(googlesheets4)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1YfS5NoVQzoR8VMHqtfdtWF3BBqeGPaxYwel4ARNofPY/edit#gid=0")

df <- read_excel("C:/Users/GudievZK/Downloads/sp.xlsx") 
colnames(df)
df <- select(df, c("ID задачи":"Заголовок"), c("Создана":"Завершенная"), c("Основание":"Поступило ли уведомление об изменениях в НПА от ЕАИСТ?"), "Дата загрузки в ЕАИСТ")
factor_vars <- c("Заголовок", "Основание", "Исполнитель ДЭПиР", "Оперативная корректировка КТД (DEPIR-Standart)",
                 "Определение (уточнение) КПГЗ и зачистки", "ОИВ (уполномоченное подведомственное учреждение)",
                 "Stage", "Вид ТЗ", "Факт создания обходных позиций КПГЗ или СПГЗ", "Лицо, давшее поручение",
                 "Факт уведомления лица, давшего поручение", "Содержатся ли изменившиеся НПА в ТЗ?",
                 "Поступило ли уведомление об изменениях в НПА от ЕАИСТ?")

df[, factor_vars] <- lapply(df[, factor_vars], factor)
_____________


remove(list = ls())
remove()


# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»
# 3. Чтобы корректно отображался текст киррилицы (в коде) необхоимо File -> Reopen with Encoding -> Show all encoding -> CP1251

#### Установка пакетов ####
install.packages('ggplot2')
library(ggplot2)
install.packages('psych')
library(psych)
install.packages("dplyr")
library(dplyr)
library(readxl)
library(xml2)
library(rvest)

#### Setwd and read csv ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

yt <- read_excel("C:/Users/GudievZK/Desktop/GitHub/DF/YT/2023.02.17_yt.xlsx")
str(yt)

page <- read_html("http://youtrack-issues.bex.su/")
yt <- html_table(page, fill = T) %>% .[[1]]

yt <- html_table(page, fill = T) %>% .[[1]] %>%
  select("ID задачи", "Заголовок", "Основание", "План по стандартизации", "Квартал", "Вид ТЗ", "Stage",
       "Количество возвратов от ДЭПиР", "Количество возвратов от ОИВ", "Исполнитель", "Руководитель группы",
       "Курирующий заместитель руководителя ПО", "Исполнитель ДЭПиР", "Контракт", "ПЦП", "Критерии оценки",
       "Форма 2", "Способы определения поставщика (подрядчика, исполнителя)", "Теги", "КТД", "Дата создания",
       "Время нахождения в статусе \"В работе АЦ\"", "Время нахождения в статусе \"Доработка ОИВ\"",
       "Время нахождения в статусе \"Доработка ДЭПиР\"", "Время нахождения в статусе \"Внут. согл.\"",
       "Время нахождения в статусе \"В ДЭПиР\"", "Время нахождения в статусе \"На согл. в ОИВ\"", 
       "Время нахождения в статусе \"Подготовка к РГ\"", "Время нахождения в статусе \"РГ\"", 
       "Время нахождения в статусе \"МРГ\"", "Время нахождения в статусе \"Загрузка в ЕАИСТ\"",
       "Длительность") %>% 
   rename(id = "ID задачи", name = "Заголовок", reason = "Основание", year_plan_st = "План по стандартизации", 
       kvartal = "Квартал", kind_tz = "Вид ТЗ", stage = "Stage", numb_ret_depir = "Количество возвратов от ДЭПиР", 
       numb_ret_oiv = "Количество возвратов от ОИВ", executor_ac = "Исполнитель", teamleader = "Руководитель группы",
       deputy = "Курирующий заместитель руководителя ПО", executor_depir = "Исполнитель ДЭПиР", contract = "Контракт", 
       pcp = "ПЦП", criteria = "Критерии оценки", f2 = "Форма 2", method = "Способы определения поставщика (подрядчика, исполнителя)", 
       tegs = "Теги", ktd = "КТД", created_date = "Дата создания", time_ac = "Время нахождения в статусе \"В работе АЦ\"", 
       time_rev_oiv = "Время нахождения в статусе \"Доработка ОИВ\"", time_rev_depir = "Время нахождения в статусе \"Доработка ДЭПиР\"", 
       time_vn_sogl = "Время нахождения в статусе \"Внут. согл.\"", time_depir = "Время нахождения в статусе \"В ДЭПиР\"", 
       time_oiv = "Время нахождения в статусе \"На согл. в ОИВ\"", time_prep_rg = "Время нахождения в статусе \"Подготовка к РГ\"", 
       time_rg = "Время нахождения в статусе \"РГ\"", time_mrg = "Время нахождения в статусе \"МРГ\"", 
       time_eaist = "Время нахождения в статусе \"Загрузка в ЕАИСТ\"", duration = "Длительность") %>% 
  slice(-c(1:3)) %>% 
  filter(!tegs %in% "Не полностью YouTrackная" & stage %in% "Завершено") %>% 
  mutate(grade_duration = ifelse(yt$duration > (mean(yt$duration, na.rm = T) + sd(yt$duration, na.rm = T)),
                                 'Bad duration', "Good duration"),
         grade_numb_ret_depir = ifelse(yt$numb_ret_depir > (mean(yt$numb_ret_depir, na.rm = T) + sd(yt$numb_ret_depir, na.rm = T)),
                                      "Bad numb_ret_depir", "Good numb_ret_depir"),
         grade_numb_ret_oiv = ifelse(yt$numb_ret_oiv > (mean(yt$numb_ret_oiv, na.rm = T) + sd(yt$numb_ret_oiv)),
                                     "Bad numb_ret_oiv", "Good numb_ret_oiv"),
         tru = case_when(startsWith(ktd, "П") ~ "Поставка товра",
                         startsWith(ktd, "В") ~ "Выполнение работ",
                         startsWith(ktd, "О") ~ "Оказание услуг"))

yt[, c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 
       'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
       'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
       'grade_numb_ret_oiv')] <- lapply(yt[, c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 
                'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
                'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
                'grade_numb_ret_oiv')], factor)
  
  
  
  
  
  
  




lapply(yt[, c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 
                'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
                'method', 'tegs', 'what_duration', 'what_numb_ret_depir', 
                'what_numb_ret_oiv', 'top_worst_ktd')], factor)
  


yt_f_names <- c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 'teamleader', 'deputy', 'contract', 
                'pcp', 'criteria', 'f2', 'method', 'tegs', 'what_duration', 'what_numb_ret_depir', 'what_numb_ret_oiv', 'top_worst_ktd')
yt[, yt_f_names] <- lapply(yt[, yt_f_names], factor)

str(yt)


#### Удаляем ненужные переменные ####

# Способ 1


#### Добавляем нужые столбцы ####
yt$what_duration <- ifelse(yt$duration > (mean(yt$duration, na.rm = T) + sd(yt$duration, na.rm = T)),
                           'Bad duration', "Good duration")

yt$what_numb_ret_depir <- ifelse(yt$numb_ret_depir > (mean(yt$numb_ret_depir, na.rm = T) + sd(yt$numb_ret_depir, na.rm = T)),
                                 "Bad numb_ret_depir", "Good numb_ret_depir")

yt$what_numb_ret_oiv <- ifelse(yt$numb_ret_oiv > (mean(yt$numb_ret_oiv, na.rm = T) + sd(yt$numb_ret_oiv)),
                               "Bad numb_ret_oiv", "Good numb_ret_oiv")

yt <- yt %>%
  mutate(tru = case_when(
    startsWith(ktd, "П") ~ "Поставка товра",
    startsWith(ktd, "В") ~ "Выполнение работ",
    startsWith(ktd, "О") ~ "Оказание услуг",
  ))





#### Меняем тип данных ####

str(yt)

yt$ktd <- as.factor(yt$ktd)
yt$reason <- as.factor(yt$reason)
yt$year_plan_st <- as.factor(yt$year_plan_st)
yt$kvartal <- as.factor(yt$kvartal)
yt$kind_tz <- as.factor(yt$kind_tz)
yt$stage <- as.factor(yt$stage)
yt$executor_ac <- as.factor(yt$executor_ac)
yt$teamleader <- as.factor(yt$teamleader)
yt$deputy <- as.factor(yt$deputy)
yt$executor_depir <- as.factor(yt$executor_depir)
yt$contract <- as.factor(yt$contract)
yt$pcp <- as.factor(yt$pcp)
yt$criteria <- as.factor(yt$criteria)
yt$f2 <- as.factor(yt$f2)
yt$method <- as.factor(yt$method)
yt$tegs <- as.factor(yt$tegs)
yt$what_duration <- as.factor(yt$what_duration)
yt$what_numb_ret_depir <- as.factor(yt$what_numb_ret_depir)
yt$what_numb_ret_oiv <- as.factor(yt$what_numb_ret_oiv)
yt$top_worst_ktd <- as.factor(yt$top_worst_ktd)

yt_f_names <- c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 'teamleader', 'deputy', 'contract', 
                'pcp', 'criteria', 'f2', 'method', 'tegs', 'what_duration', 'what_numb_ret_depir', 'what_numb_ret_oiv', 'top_worst_ktd')
yt[, yt_f_names] <- lapply(yt[, yt_f_names], factor)

str(yt)

  

