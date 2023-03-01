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
library(stringr)
library(tidyr)
library(tibble)

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
  filter(!tegs %in% "Не полностью YouTrackная" & stage %in% "Завершено")
  

# Заменяем пустые значения в столбце ktd на номенования КТД
yt$ktd[yt$ktd == ""] <- filter(yt, ktd == "")$name

# Добавляем столбцы

yt <- yt %>% mutate(grade_duration = ifelse(duration > (mean(duration, na.rm = T) + sd(duration, na.rm = T)),
                               'Bad duration', "Good duration"),
             grade_numb_ret_depir = ifelse(numb_ret_depir > (mean(numb_ret_depir, na.rm = T) + sd(numb_ret_depir, na.rm = T)),
                                     "Bad numb_ret_depir", "Good numb_ret_depir"),
             grade_numb_ret_oiv = ifelse(numb_ret_oiv > (mean(numb_ret_oiv, na.rm = T) + sd(numb_ret_oiv)),
                                   "Bad numb_ret_oiv", "Good numb_ret_oiv"),
             tru = case_when(startsWith(ktd, "П") ~ "Поставка товра",
                       startsWith(ktd, "В") ~ "Выполнение работ",
                       startsWith(ktd, "О") ~ "Оказание услуг"))

# Меняем тип некоторых переменных на фактор

yt[, c('reason', 'year_plan_st', 'kvartal', 'kind_tz', 'executor_depir','stage', 'executor_ac', 
       'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
       'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
       'grade_numb_ret_oiv', 'tru')] <- lapply(yt[, c('reason', 'year_plan_st', 'kvartal', 'kind_tz', 'executor_depir','stage', 'executor_ac', 
                                                      'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
                                                      'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
                                                      'grade_numb_ret_oiv', 'tru')], as.factor)
# Меняем тип created_date на date

yt$created_date <- str_extract_all(yt$created_date, "^.{10}") %>% unlist() # удаляем время из переменной created_date

yt$created_date <- as.Date(yt$created_date)

# Меняем тип year_plan_st с на Ordered factor
yt$year_plan_st <- ordered(yt$year_plan_st)

# Определяем задачи с некорректными данными

## Заменяем NA на 0 в переменной time_ac, time_rev_oiv, time_rev_depir
yt[, c("time_ac", "time_rev_depir", "time_rev_oiv")] <- replace_na(select(yt, time_ac, time_rev_depir, time_rev_oiv),
                                                                list(time_ac = 0, time_rev_depir = 0, time_rev_oiv = 0))
                                                      
## Определяем КТД, в которых время нахождения time_ac < 5 или time_rev_depir < 1 или time_rev_oiv < 1
suspicious_time <- filter(yt, time_ac <= 5 | time_rev_depir < 1 | time_rev_oiv < 3)

## Определяем КТД, которые не разу не возвращались на доработку ДЭПиР или ОИВ 
suspicious_ret <- filter(yt, numb_ret_depir < 1 | numb_ret_oiv < 1)

## Соединяем таблицы

suspicious_ktd <- rbind(suspicious_time, suspicious_ret)

## Добавляем столбцы для заполнения

suspicious_ktd <- suspicious_ktd %>% 
  add_column(Comment_numb_ret_depir = ifelse(suspicious_ktd$numb_ret_depir == 0, 
                                             "Подтвердить отсутсвие замечаний ДЭПиР или прокомментривать отсутствие данной информации в YT", 
                                             "-"),
             .after = "numb_ret_depir"
             ) %>% 
  add_column(Comment_numb_ret_oiv = ifelse(suspicious_ktd$numb_ret_oiv == 0, 
                                           "Подтвердить отсутсвие замечаний ОИВ или прокомментривать отсутствие данной информации в YT", 
                                           "-"),
             .after = "numb_ret_oiv"
             ) %>% 
  add_column(Comment_time_ac = ifelse(suspicious_ktd$time_ac <= 5,
                                            "Подтвердить время разработки КТД или в случае некорректности срока разработки, указать причину из-за которой данный срок в YT некорректен", 
                                            "-"),
             .after = "time_ac"
             ) %>% 
  add_column(Comment_time_rev_depir = ifelse(suspicious_ktd$time_rev_depir < 1,
                                           "Подтвердить время доработки КТД по замечаниям ДЭПиР или в случае некорректности доработки КТД по замечаниям ОИВ, указать причину из-за которой данный срок в YT некорректен", 
                                           "-"),
             .after = "time_rev_depir"
            ) %>% 
  add_column(Comment_time_rev_oiv = ifelse(suspicious_ktd$time_rev_oiv < 3,
                                           "Подтвердить время доработки КТД по замечаниям ОИВ или в случае некорректности доработки КТД по замечаниям ОИВ, указать причину из-за которой данный срок в YT некорректен", 
                                           "-"),
             .after = "time_rev_oiv"
                              )

## Сохраняем подозрительные КТД
names(suspicious_ktd) %>% paste(sep = ", ")

suspicious_ktd %>% 
  select("id", "name", "reason", "year_plan_st", "kvartal", "kind_tz", "stage", "numb_ret_depir", "Comment_numb_ret_depir", "numb_ret_oiv", 
         "Comment_numb_ret_oiv", "executor_ac", "teamleader", "deputy", "executor_depir", "contract", "pcp", "criteria", "f2", "method", "tegs",
         "ktd", "created_date", "time_ac", "Comment_time_ac", "time_rev_oiv", "Comment_time_rev_oiv", "time_rev_depir", "Comment_time_rev_depir", 
         "time_vn_sogl", "time_depir", "time_oiv", "time_prep_rg", "time_rg", "time_mrg", "time_eaist", "duration", "tru") %>% 
  write_sheet(
    ss = gs4_get(
      "https://docs.google.com/spreadsheets/d/1ZZyeNdpO3IPbPXVgWt6rNMFgzRQPZ_LRhbW9Srq0QDg/edit#gid=0"
    ),
    sheet = "Лист1"
  )








filter(yt, is.na(yt$time_rev_oiv) & time_ac < 10)[["id"]] %>% 
  paste(collapse = ", ")

# Записываем получившийся дата фрейм в гул таблицу

yt %>%
  write_sheet(
    ss = gs4_get(
      "https://docs.google.com/spreadsheets/d/1hXDJbsgdGZdjXxkxcaKKNJk9EwhP2CIPNaY5NilxkvY/edit?usp=sharing" # Replace the access link to the spreadsheets
    ),
    sheet = "list1"
  )


