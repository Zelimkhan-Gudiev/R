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

# �������� ������
df_origin <- read_excel("C:/Users/GudievZK/Desktop/GitHub/AC/DA-21 ���������������� � ���������������� ������� ��� ��� ����������� ����/2024.06.19_DA-21__MKR_����� �� �������� �� 2023.xlsx")

# ���������������� �������
colnames(df_origin) <- c("kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date",
                  "using_tz", "notice_publication_date", "subject_of_procurement", "contract_date", "lot_id",
                  "contract_eis_registry_number", "contract_completion_date", "law", "method_of_determinig_supplier",
                  "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
                  "sum_of_contracts")

# ���������� ������ �����
df_origin <- fill(df_origin, "kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date")

# �������� ������ �������� � �����
df_origin <- df_origin[-c(1, 2),]
df_origin <- filter(df_origin, str_detect(df_origin$kod_kpgz, "^01.15")) # ������� ������� ����, �� ������������ � 01.15


# ��������� ������ �� �������� "���������"

df_origin <- filter(df_origin, object_status!="���������" | !is.na(subject_of_procurement))

df_origin %>% filter(is.na(subject_of_procurement))

is.na(df$subject_of_procurement) %>% sum()

# ��������� ����� ����������
df_origin$is_oiv <- ifelse(!str_detect(df_origin$customer, "^��|^\"��|^��|^��|^���|^���|^�����|^����|^���|^����|^����|
                                  |^�� � ���|^���|^���� ������ ��������|^��|^����|^����|^���|^�����|
                                  |^���|^�� |^��� |^���������� ��������������� ������������ �����-����������|
                                  |^���� ���������|^���� ������������� ������, ����������� ������������� ������|
                                  |^���������� ���� ������ ���� ���������|^����� ��. �. ��������|
                                  |^����������� ����� ������������|^���"), 'oiv', 'other')


df_origin$has_contract_eis_registry_number <- ifelse(!df_origin$contract_eis_registry_number == "-", "yes", "no")
df_origin$has_for <- ifelse(str_detect(df_origin$subject_of_procurement, '���.*|� ����.*|� ������.*|� �����.*|���������.*|����������.*|�����.*'), 
                     "for needs", "other")
df_origin$subject_of_procurement <- tolower(df_origin$subject_of_procurement)
df_origin$for_whose_needs <- str_extract(df_origin$subject_of_procurement, '���.*|� ����.*|� ������.*|� �����.*|���������.*|����������.*|�����.*') %>% 
  str_replace("��� ���� ", "")
df_origin$needs_owner <- ifelse(str_detect(df_origin$for_whose_needs, '����|����.*|�����.*|�� |���������.*|�����.*|�����.*|�����.*|������.|�������.*'), 
         NA, df_origin$for_whose_needs)
  
# ���������� ������ �� contract_eis_registry_number � lot_id

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


# ������ ��� ������ ��������� ���������� �� ������
factor_var <- c("kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "name_tz",
                "using_tz", "law", "method_of_determinig_supplier", 
                "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
                "sum_of_contracts", "is_oiv", "has_contract_eis_registry_number", "has_for")

df[, factor_var] <- lapply(df[, factor_var], factor)
str(df)


# ������ ��. ���������
df_origin$sum <- df_origin$sum * 1000

# ������� ������ �������

df <- df %>% select(grbs, customer, supplier, law, notice_publication_date, subject_of_procurement,
              contract_date, lot_id, contract_eis_registry_number, contract_completion_date,
              law, method_of_determinig_supplier, Basis_for_concluding_a_contract_with_a_single_supplier,
              is_oiv, has_contract_eis_registry_number, has_for, for_whose_needs, needs_owner, sum)

colnames(df)
# table(df$is_oiv), df$has_for)


#### ������ ������
df_oiv <- filter(df, is_oiv == "oiv")
corpus <- Corpus(VectorSource(df_oiv$needs_owner))
corpus[[1]][1]

# ��������� ����� � ������ �������
corpus <- tm_map(corpus, content_transformer(tolower))
# ������� �����
corpus <- tm_map(corpus, removeNumbers)
### ������� ���� �����
corpus <- tm_map(corpus, removeWords, stopwords("russian"))
corpus <- tm_map(corpus, removeWords, c("����", "���", "������", "������", "�", "N", "�������", "����������",  
                                        "����������", "�����", "�����", "����"))

corpus$content <- str_replace_all(corpus$content, "^�����|^����|^ �������|^ �������|
                                  |^�������|^�����", "")
# Step 3: Define a function to remove words starting with certain patterns
remove_words_starting_with <- function(text, patterns) {
  # Create a regex pattern to match words starting with any of the specified patterns
  regex_pattern <- paste0("\\b(", paste(patterns, collapse = "|"), ")\\w*\\b")
  # Replace the matching words with an empty string
  str_replace_all(text, regex_pattern, "")
}

# Step 4: Specify the patterns and apply the function to the corpus
patterns <- c("�����", "����", "����", "������", "��", "������", "�")  # Patterns to match the start of words
corpus <- tm_map(corpus, content_transformer(function(text) remove_words_starting_with(text, patterns)))

# Step 5: Inspect the cleaned corpus
inspect(corpus)

### ������� ����� ����������
corpus <- tm_map(corpus, removePunctuation)
# ������� ������ �������
corpus <- tm_map(corpus, stripWhitespace)

# ������� ���
tdm <- TermDocumentMatrix(corpus)
mtrx <- as.matrix(tdm)
v <- sort(rowSums(mtrx), decreasing = T)
df_mtrx <- data.frame(word=names(v),freq=v)

wordcloud(df_mtrx$word, df_mtrx$freq, colors = brewer.pal(6,"Dark2"))


##### ���� #####
corpus[[1]][1]
str(corpus)

write_xlsx(df, "C:/Users/GudievZK/Desktop/GitHub/AC/DA-21 ���������������� � ���������������� ������� ��� ��� ����������� ����/DA-21 df_temp.xlsx")


# str_extract(df$subject_of_procurement, '���.*|� ����.*|� ������.*|� �����.*|���������.*') %>% str_replace("��� ���� ", "")

remove(df_has_contract,df_has_not_contract, factor_var)


