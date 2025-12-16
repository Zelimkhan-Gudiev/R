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
library(writexl)
library(stringr)
library(vroom)

df <- read.csv("C:/Users/GudievZK/Nextcloud/Data analysis/DA-54/2025.12.16_spgz_kpgz_ktru_pack_rels_db_ktlg.csv", fileEncoding = "Windows-1251")
df <- clean_names(df)
names(df)
glimpse(df)
str(df)
sapply(df, class)
colSums(is.na(df))

toString(names(df))

df_without_id_entity_spgz_eaist <- df %>% filter(is.na(id_entity_spgz_eaist))
sapply(df, function(x) sum(duplicated(x)))
sapply(df, function(x) sum(n_distinct(x)))

df <- df %>% select(id_entity_spgz_katalog, id_version_spgz_katalog, name_spgz, 
                    isstandardized_spgz, isproject_spgz, isdeleted_spgz, 
                    isexcluded_spgz, isemias_spgz, ispcp_spgz, createdate_spgz, editdate_spgz, 
                    begindate_version_spgz, enddate_version_spgz, okpd2_code_spgz, okpd2_name_spgz, 
                    ktru_code_spgz, ktru_name_spgz, oldpgzid, linktype, newpgzid, isactual, 
                    previouspgzid, sourcepgzid, reasonid, name,name_kpgz, code_kpgz)

df_1 <- df %>% filter(
  id_entity_spgz_katalog == 29152105)

df_1 <- df_1 %>% select(id_entity_spgz_katalog, id_version_spgz_katalog, name_spgz,
                createdate_spgz, editdate_spgz, 
                begindate_version_spgz, enddate_version_spgz,
                oldpgzid, linktype, newpgzid, isactual, sourcepgzid, 
                pgzid, reasonid, name,name_kpgz, code_kpgz)

write_xlsx(df_1, "C:/Users/GudievZK/Nextcloud/Data analysis/DA-54/2025.12.15/2025.12.16 df_1.xlsx")



