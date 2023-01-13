install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(dplyr)
library("writexl")
library(tidyr)

########## Считывание данных
exel_book <- readxl::excel_sheets("C:/Users/GudievZK/Desktop/Новая папка/2022.12.06 ВыгрузкаВКРиС.xlsx") %>% 
             sapply(., read_excel, path = "C:/Users/GudievZK/Desktop/Новая папка/2022.12.06 ВыгрузкаВКРиС.xlsx")
      
df <- left_join(exel_book[["Отчет"]], 
      exel_book[["данные Столянкова"]][, c(1,2)], 
      by = "CODE")

r <- exel_book[['расшифровка данных']]


########## Предобработка



df <- rename(df, dS1 = `данные Столянкова`,
                 dS2 = `Данные Столянкова`,
                 isFinal = `не конечный`)

df$dS1 <- replace_na(df$dS1, 0)
df$dS2 <- replace_na(df$dS2, 0)
df <- select(df, - dS2)

df <- rename(df, dS= dS1)


filter(df, dS1 == 4)


df$IS_STANDARD_PRODUCT[df$IS_STANDARD_PRODUCT == "0"] <- "Нестандартизирована"
df$IS_STANDARD_PRODUCT[df$IS_STANDARD_PRODUCT == "1"] <- "Стандартизирована"
df$IS_STANDARD_PRODUCT[df$dS == 1] <- "Удалена и стандартизирована"
df$IS_STANDARD_PRODUCT[df$dS == 2] <- "Будет детализирована и стандартизирована"
df$IS_STANDARD_PRODUCT[df$dS == 3] <- "Детализирована и стандартизирована"
df$IS_STANDARD_PRODUCT[df$IS_STANDARD_PRODUCT == 3]  <- "Детализирована и стандартизирована"
df$IS_STANDARD_PRODUCT[df$dS == 4] <- "Будет стандартизирована"
df$IS_STANDARD_PRODUCT[df$dS == 5] <- "Будут стандартизированы дочерение КПГЗ"
df$IS_STANDARD_PRODUCT[df$dS == 6] <- "Нерегулярная"


colnames(df)
str(df)
glimpse(df)

df[c("isFinal", "IS_STANDARD_PRODUCT", "dS1" )] <- c("isFinal", "IS_STANDARD_PRODUCT", 
                                                    "dS") %>% df[.] %>% lapply(., factor)

df %>% 
  group_by(IS_STANDARD_PRODUCT) %>% 
  summarise(sum(KPGZ_SUM22, na.rm = T)) %>% arrange(`sum(KPGZ_SUM22, na.rm = T)`)

(sum(6342637259, 559350175381, 18238958265))/(sum(df$KPGZ_SUM22, na.rm = T)) * 100

write_xlsx(r, "C:/Users/GudievZK/Desktop/Новая папка/r.xlsx")





