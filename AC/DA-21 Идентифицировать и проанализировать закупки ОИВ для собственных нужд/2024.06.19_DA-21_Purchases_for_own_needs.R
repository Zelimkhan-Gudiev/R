library(readxl)
library(writexl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)
library(rlang)
# install.packages(c("tm", "wordcloud","SnowballC"))
library(tm)
library(wordcloud)
library(SnowballC)
# install.packages("tidytext")
library(tidytext)
# install.packages("RColorBrewer")
library(RColorBrewer)

# Загрузка данных
df_origin <- read_excel("C:/Users/GudievZK/Desktop/GitHub/AC/DA-21 Идентифицировать и проанализировать закупки ОИВ для собственных нужд/2024.06.19_DA-21__MKR_Отчет по закупкам за 2023.xlsx")

# Переименовывание колонок
colnames(df_origin) <- c("kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date",
                  "using_tz", "notice_publication_date", "subject_of_procurement", "contract_date", "lot_id",
                  "contract_eis_registry_number", "contract_completion_date", "law", "method_of_determinig_supplier",
                  "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
                  "sum_of_contracts")

# Заполнение пустых ячеек
df_origin <- fill(df_origin, "kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date")

# Удаление лишних столбцов и строк
df_origin <- df_origin[-c(1, 2),]
df_origin <- filter(df_origin, str_detect(df_origin$kod_kpgz, "^01.15")) # удаляем позиции КПГЗ, не начинающиеся с 01.15


# Исключаем строки со статусом "Процедуры"

df_origin <- filter(df_origin, object_status!="Процедуры" | !is.na(subject_of_procurement))

df_origin %>% filter(is.na(subject_of_procurement))

is.na(df$subject_of_procurement) %>% sum()

# Добавляем новые переменные
df_origin$is_oiv <- ifelse(!str_detect(df_origin$customer, "^ГБ|^\"ГБ|^ГК|^АО|^ГАУ|^ГУП|^ГАПОУ|^ГАОУ|^ООО|^«НИИ|^МАОУ|
                                  |^СС и НМП|^ОАО|^«Мой особый семейный|^КП|^БЮРО|^ГПБУ|^МБУ|^МАДОУ|
                                  |^МАУ|^МУ |^МУК |^МОСКОВСКИЙ ГОСУДАРСТВЕННЫЙ ОБЪЕДИНЕННЫЙ МУЗЕЙ-ЗАПОВЕДНИК|
                                  |^Фонд реновации|^МГУУ Правительства Москвы, Университет Правительства Москвы|
                                  |^Московский фонд защиты прав дольщиков|^ЦПКиО им. М. Горького|
                                  |^«Московский театр «Современник»|^МКУ"), 'oiv', 'other')


df_origin$has_contract_eis_registry_number <- ifelse(!df_origin$contract_eis_registry_number == "-", "yes", "no")
df_origin$has_for <- ifelse(str_detect(df_origin$subject_of_procurement, 'для.*|с цель.*|в рамках.*|в целях.*|обеспечен.*|направленн.*|управ.*'), 
                     "for needs", "other")
df_origin$subject_of_procurement <- tolower(df_origin$subject_of_procurement)
df_origin$for_whose_needs <- str_extract(df_origin$subject_of_procurement, 'для.*|с цель.*|в рамках.*|в целях.*|обеспечен.*|направленн.*|управ.*') %>% 
  str_replace("для нужд ", "")
df_origin$needs_owner <- ifelse(str_detect(df_origin$for_whose_needs, 'опоп|офис.*|совет.*|св |мероприят.*|выбор.*|помощ.*|льгот.*|жителя.|избират.*'), 
         NA, df_origin$for_whose_needs)
  
# Группируем строки по contract_eis_registry_number и lot_id

df_has_contract <- df_origin %>% 
  filter(has_contract_eis_registry_number == "yes") %>% 
  group_by(contract_eis_registry_number) %>% 
  mutate(sum = sum(sum_of_contracts)) %>% 
  distinct(contract_eis_registry_number, .keep_all = T)


df_has_not_contract <- df_origin %>% 
  filter(has_contract_eis_registry_number == "no") %>% 
  group_by(lot_id, subject_of_procurement, contract_date, supplier) %>% 
  mutate(sum = sum(sum_of_contracts)) %>% 
  distinct(lot_id, subject_of_procurement, .keep_all = T)

df <- combine(df_has_contract, df_has_not_contract)


# Меняем тип данных некоторых переменных на Фактор
factor_var <- c("kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "name_tz",
                "using_tz", "law", "method_of_determinig_supplier", 
                "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
                "sum_of_contracts", "is_oiv", "has_contract_eis_registry_number", "has_for")

df[, factor_var] <- lapply(df[, factor_var], factor)
str(df)


# Меняем ед. измерения
df_origin$sum <- df_origin$sum * 1000

# Удаляем лишние столбцы

df <- df %>% select(grbs, customer, supplier, law, notice_publication_date, subject_of_procurement,
              contract_date, lot_id, contract_eis_registry_number, contract_completion_date,
              law, method_of_determinig_supplier, Basis_for_concluding_a_contract_with_a_single_supplier,
              is_oiv, has_contract_eis_registry_number, has_for, for_whose_needs, needs_owner, sum)

colnames(df)
# table(df$is_oiv), df$has_for)


#### Анализ текста
df_oiv <- filter(df, is_oiv == "oiv")
corpus <- Corpus(VectorSource(df_oiv$needs_owner))
corpus[[1]][1]

# Переводим текст в нижний регистр
corpus <- tm_map(corpus, content_transformer(tolower))
# Удаляем цифры
corpus <- tm_map(corpus, removeNumbers)
### Удаляем стоп слова
corpus <- tm_map(corpus, removeWords, stopwords("russian"))
corpus <- tm_map(corpus, removeWords, c("году", "год", "москвы", "города", "№", "N", "закупка", "совместная",  
                                        "совместные", "торги", "среди", "нужд"))

corpus$content <- str_replace_all(corpus$content, "^бумаг|^цвет|^ офисной|^ техники|
                                  |^техники|^москв", "")
# Step 3: Define a function to remove words starting with certain patterns
remove_words_starting_with <- function(text, patterns) {
  # Create a regex pattern to match words starting with any of the specified patterns
  regex_pattern <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\w*\\b")
  # Replace the matching words with an empty string
  str_replace_all(text, regex_pattern, "")
}

# Step 4: Specify the patterns and apply the function to the corpus
patterns <- c("бумаг", "цвет", "офис", "техник", "ой", "формат", "№")  # Patterns to match the start of words
corpus <- tm_map(corpus, content_transformer(function(text) remove_words_starting_with(text, patterns)))

# Step 5: Inspect the cleaned corpus
inspect(corpus)

### Удаляем знаки припенания
corpus <- tm_map(corpus, removePunctuation)
# Удаляем лишние пробелы
corpus <- tm_map(corpus, stripWhitespace)

# Создаем ТДМ
tdm <- TermDocumentMatrix(corpus)
mtrx <- as.matrix(tdm)
v <- sort(rowSums(mtrx), decreasing = T)
df_mtrx <- data.frame(word=names(v),freq=v)

wordcloud(df_mtrx$word, df_mtrx$freq, colors = brewer.pal(6,"Dark2"))


##### ТЕСТ #####
corpus[[1]][1]
str(corpus)

write_xlsx(df, "C:/Users/GudievZK/Desktop/GitHub/AC/DA-21 Идентифицировать и проанализировать закупки ОИВ для собственных нужд/DA-21 df_temp.xlsx")


# str_extract(df$subject_of_procurement, 'для.*|с цель.*|в рамках.*|в целях.*|обеспечен.*') %>% str_replace("для нужд ", "")

remove(df_has_contract,df_has_not_contract, factor_var)


