library(readxl)
library(dplyr)

zakupki <- read_xlsx("mkr_jan2023.xlsx", sheet = "zakupki")

agg_def <- zakupki %>%
  ## строку ниже нужно раскомментировать, если нужно отфильтровать незаполенные наименования
  # filter(!is.na(`Наименование`)) %>%
  
  # оставляем только уникальные сочетания трех полей
  distinct(`Реестровый номер контракта/договора/закупки в ЕАИСТ`,
           `Наименование`,
           `Способ определения поставщика`) %>%
  
  # группируем по `Реестровый номер...`
  group_by(`Реестровый номер контракта/договора/закупки в ЕАИСТ`) %>%
  
  # считаем количество уникальных наименований для каждого реестрового номера
  mutate(uniq_names = n_distinct(`Наименование`)) %>%
  
  # отфильтровываем записи, где количество уникальных наименований для каждого
  # реестрового номера > 1
  filter(uniq_names > 1) %>%
  
  # сортируем по убыванию кол-ва уникальных наименований и реестровым номерам
  arrange(desc(uniq_names), `Реестровый номер контракта/договора/закупки в ЕАИСТ`) %>%
  
  # оставляем только необходимые колонки
  select(`Реестровый номер контракта/договора/закупки в ЕАИСТ`,
         `Наименование`,
         `Способ определения поставщика`)

agg_def
