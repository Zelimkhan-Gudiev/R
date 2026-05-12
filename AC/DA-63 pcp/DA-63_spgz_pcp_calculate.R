
library(psych)
library(googlesheets4)
library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)
library(purrr)
library(GGally)
library(readxl)
library(skimr)
library(writexl)
library(corrplot)


df_src <- read_excel("C:/Users/GudievZK/Nextcloud/Data analysis/DA-63/CÏÃÇ_27_02_2026 (all_p).xlsx", skip = 3) %>% slice(-1)

names(df)

df_src <- clean_names(df_src)

str(df_src)
summary(df$est_pcp)

skim(df_src)

toString(names(df))

df <- df_src %>% fill(identifikator_spgz, naimenovanie_spgz, kpgz, 
                  okpd_2, standartizirovana, ktru, 
                  edinicy_izmerenia, paket, status, 
                  aktual_na, data_poslednego_izmenenia, 
                  udalena, est_pcp, .direction = "down")

df %>% group_by()

df %>% slice(1:1000) %>% write_xlsx("C:/Users/GudievZK/Nextcloud/Data analysis/DA-63/spgz_1000rows.xlsx")
