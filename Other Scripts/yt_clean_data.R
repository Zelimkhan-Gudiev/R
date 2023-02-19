# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»
# 3. Чтобы корректно отображался текст киррилицы (в коде) необхоимо File -> Reopen with Encoding -> Show all encoding -> CP1251

#### Подключение пакетов ####

library(dplyr)
library(readxl)
library(xml2)
library(rvest)
library(htmlTable)
library(googlesheets4)
library(rvest)
library(writexl)

#### Setwd and clean data ####

yt <- read_html("http://youtrack-issues.bex.su/") %>% html_table(fill = T) %>% .[[1]] %>%
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
  mutate(grade_duration = ifelse(duration > (mean(duration, na.rm = T) + sd(duration, na.rm = T)),
                                 'Bad duration', "Good duration"),
         grade_numb_ret_depir = ifelse(numb_ret_depir > (mean(numb_ret_depir, na.rm = T) + sd(numb_ret_depir, na.rm = T)),
                                      "Bad numb_ret_depir", "Good numb_ret_depir"),
         grade_numb_ret_oiv = ifelse(numb_ret_oiv > (mean(numb_ret_oiv, na.rm = T) + sd(numb_ret_oiv)),
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
                                                'grade_numb_ret_oiv')], as.factor)

yt[yt$ktd == "", ]$ktd <- yt[yt$ktd == "", ]$name

yt <- yt %>% 
         mutate(grade_duration = ifelse(duration > (mean(duration, na.rm = T) + sd(duration, na.rm = T)),
                                 'Bad duration', "Good duration"),
         grade_numb_ret_depir = ifelse(numb_ret_depir > (mean(numb_ret_depir, na.rm = T) + sd(numb_ret_depir, na.rm = T)),
                                       "Bad numb_ret_depir", "Good numb_ret_depir"),
         grade_numb_ret_oiv = ifelse(numb_ret_oiv > (mean(numb_ret_oiv, na.rm = T) + sd(numb_ret_oiv)),
                                     "Bad numb_ret_oiv", "Good numb_ret_oiv"),
         tru = case_when(startsWith(ktd, "П") ~ "Поставка товара",
                         startsWith(ktd, "В") ~ "Выполнение работ",
                         startsWith(ktd, "О") ~ "Оказание услуг"))
yt$reason %>% levels()

df <- yt %>% 
        filter(reason %in% c("План по стандартизации","Поручение ДЭПиР или руководства"), kind_tz == "ТТЗ") %>% 
        group_by(kind_tz, tru, year_plan_st, deputy) %>% 
        summarise(mean_duration = mean(duration),
                  sd_duration = sd(duration),
                  numb_ret_depir = mean(numb_ret_depir),
                  sd_numb_ret_depir = sd(numb_ret_depir),
                  numb_ret_oiv = mean(numb_ret_oiv),
                  sd_numb_ret_oiv = sd(numb_ret_oiv),
                  n = length(tru)) %>% 
        arrange(deputy, tru, year_plan_st)

