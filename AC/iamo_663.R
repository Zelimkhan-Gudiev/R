install.packages("psych")
install.packages("lubridate")
install.packages("purrr")
install.packages("GGally")
install.packages("writexl")
install.packages("stringr")
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

#### Reading data
kpgz_db_katalog <- read_excel("C:/Users/GudievZK/Desktop/KatalogDB/IAMO_663/2025.09.29_data.xlsx", sheet = "_kpgz_db_katalog_src")
kpgz_nsi <- read_excel("C:/Users/GudievZK/Desktop/KatalogDB/IAMO_663/2025.09.29_data.xlsx", sheet = "2025.09.29_kpgz_nsi_src")
tz_bktz <- read_excel("C:/Users/GudievZK/Desktop/KatalogDB/IAMO_663/2025.09.29_data.xlsx", sheet = "2025.09.29_tz_bktz_src")

#### Wieving data stracture
str(kpgz_db_katalog)
str(kpgz_nsi)
str(tz_bktz)

########## Processing data (Step 3) ##########

########## Changing variables names

######### kpgz_db_katalog

kpgz_db_katalog <- clean_names(kpgz_db_katalog)

colnames(kpgz_db_katalog) <- paste0(
  colnames(kpgz_db_katalog), "_db_ktlg")

######### kpgz_nsi

kpgz_nsi <- clean_names(kpgz_nsi)
colnames(kpgz_nsi) <- paste0(
  colnames(kpgz_nsi), "_nsi")

######### tz_bktz

tz_bktz <- clean_names(tz_bktz)
colnames(tz_bktz) <- paste0(
  colnames(tz_bktz), "_bktz")

tz_bktz <- tz_bktz %>% 
  rename(naimenovanie_tz_bktz = naimenovanie_bktz)

# Looking structure
str(kpgz_db_katalog)
str(kpgz_nsi)
str(tz_bktz)

########## Transforming data types ########## 
##########  kpgz_db_katalog
kpgz_db_katalog <- kpgz_db_katalog %>% 
  modify_if(is.numeric, as.character)

kpgz_db_katalog <- kpgz_db_katalog %>% 
  mutate(across(where(~ is.character(.) | is.numeric(.)),
                ~ if(n_distinct(.) < 100) as.factor(.) else .))

kpgz_db_katalog <- kpgz_db_katalog %>% 
  mutate(createdate_kpgz_db_ktlg = as.POSIXct(createdate_kpgz_db_ktlg),
         enddate_kpgz_db_ktlg = as.POSIXct(enddate_kpgz_db_ktlg),
         okpd2_canceldate_kpgz_db_ktlg = as.POSIXct(okpd2_canceldate_kpgz_db_ktlg),
         approvaldate_kpgz_db_ktlg = as.POSIXct(approvaldate_kpgz_db_ktlg))


##########  kpgz_nsi
kpgz_nsi <- kpgz_nsi %>% 
  mutate(across(where(is.numeric), as.character))

kpgz_nsi <- kpgz_nsi %>% 
  mutate(across(where(~ is.character(.) | is.numeric(.)),
               ~ if(n_distinct(.) < 100) as.factor(.) else .))

kpgz_nsi <- kpgz_nsi %>% 
  mutate(across(c(data_i_vrema_sozdania_nsi,
                  data_poslednego_izmenenia_nsi,
                  data_udalenia_nsi),
                ~ dmy_hms(.)))

##########  tz_bktz

tz_bktz <- tz_bktz %>% 
  fill(naimenovanie_tz_bktz, 
       status_bktz,
       strukturirovannoe_tz_bktz,
       obazatel_nost_ispol_zovania_bktz,
       data_poslednego_izmenenia_bktz,
       data_utverzdenia_bktz,
       .direction = "down")

tz_bktz <- tz_bktz %>% 
  modify_if(is.numeric, as.character)

tz_bktz <- tz_bktz %>% 
  modify_if(~ is.character(.) && n_distinct(.) < 100, as.factor)

tz_bktz <- tz_bktz %>% 
  mutate(
    data_poslednego_izmenenia_bktz = as.POSIXct(data_poslednego_izmenenia_bktz),
    data_utverzdenia_bktz = as.POSIXct(data_utverzdenia_bktz),
    code_kpgz_bktz_level = str_count(kpgz_bktz_code, "\\d") / 2)

tz_bktz <- tz_bktz %>% 
              mutate(
                kpgz_bktz_code = str_split(kpgz_bktz, " ", n = 2, simplify = TRUE)[,1],
                kpgz_bktz_name = str_split(kpgz_bktz, " ", n = 2, simplify = TRUE)[,2]
                )  
  
########## Filtering data ##########
########## kpgz_nsi
kpgz_nsi <- kpgz_nsi %>% 
  filter(status_nsi == "Утвержден") # & !str_starts(kod_kpgz_nsi, "01.02"))
str(kpgz_nsi)


########## kpgz_db_katalog

########## tz_bktz

tz_bktz <- tz_bktz %>% 
  filter(status_bktz == "Утвержден")

########## Merging tables ##########

df_full <- kpgz_nsi %>% 
  left_join(kpgz_db_katalog, 
            by = c("kod_kpgz_nsi" = "code_kpgz_db_ktlg"), keep = TRUE) %>% # by = c("identifikator_nsi" = "id_entity_kpgz_eaist_db_ktlg"), keep = TRUE)
  left_join(tz_bktz,
            by = c("kod_kpgz_nsi" = "kpgz_bktz_code"), keep = TRUE) %>%
  select(
    identifikator_versii_nsi,
    identifikator_kpgz_v_kataloge_nsi,
    identifikator_versii_kpgz_v_kataloge_nsi,
    data_i_vrema_sozdania_nsi,
    data_poslednego_izmenenia_nsi,
    kod_kpgz_nsi,
    naimenovanie_kpgz_nsi,
    vysestoasij_kpgz_nsi,
    opisanie_nsi,
    okpd_nsi,
    okpd_2_nsi,
    kod_nomenklatury_mer_nsi,
    kod_pozicii_ktru_nsi,
    neobhodimost_privazki_k_ob_ektu_gorodskogo_hozajstva_nsi,
    priznak_standartizirovannoj_pozicii_nsi,
    trebuetsa_provedenie_gosudarstvennoj_ekspertizy_nsi,
    otnositsa_k_kapital_nomu_i_tekusemu_remontu_blagoustrojstvu_ustrojstvu_pesehodnyh_zon_snosu_i_t_p_nsi,
    tip_ob_ekta_gorodskogo_hozajstva_nsi,
    est_pcp_nsi,
    data_udalenia_nsi,
    status_nsi,
    naimenovanie_sablona_tz_nsi,
    id_entity_kpgz_eaist_db_ktlg,
    id_kpgz_eaist_db_ktlg,
    id_entity_kpgz_katalog_db_ktlg,
    id_version_kpgz_katalog_db_ktlg,
    parentid_kpgz_db_ktlg,
    code_kpgz_db_ktlg,
    name_kpgz_db_ktlg,
    isstandardized_kpgz_db_ktlg,
    isproject_kpgz_db_ktlg,
    isdeleted_kpgz_db_ktlg,
    isexcluded_kpgz_db_ktlg,
    isemias_kpgz_db_ktlg,
    ispcp_kpgz_db_ktlg,
    attributetarif_kpgz_db_ktlg,
    ktruid_kpgz_db_ktlg,
    okpdid_kpgz_db_ktlg,
    iskpgz_kpgz_db_ktlg,
    createdate_kpgz_db_ktlg,
    enddate_kpgz_db_ktlg,
    okpd2_id_kpgz_db_ktlg,
    last_kpgz_db_ktlg,
    okpd2_code_kpgz_db_ktlg,
    okpd2_name_kpgz_db_ktlg,
    okpd2_canceldate_kpgz_db_ktlg,
    okpd2_isactual_kpgz_db_ktlg,
    okpd2_okpd2id_kpgz_db_ktlg,
    ktru_code_kpgz_db_ktlg,
    ktru_name_kpgz_db_ktlg,
    id_version_kpgz_katalog_in_package_db_ktlg,
    packageid_db_ktlg,
    approvaldate_kpgz_db_ktlg,
    naimenovanie_tz_bktz,
    kpgz_bktz,
    status_bktz,
    strukturirovannoe_tz_bktz,
    obazatel_nost_ispol_zovania_bktz,
    data_poslednego_izmenenia_bktz,
    data_utverzdenia_bktz,
    kpgz_bktz_code,
    kpgz_bktz_name
  )


########## Checking data frames ##########
########## kpgz_nsi

kpgz_nsi_dups_na <- kpgz_nsi %>%
  map_df(~ {
    values <- .
    dup_mask <- duplicated(values) | duplicated(values, fromLast = TRUE)
    na_count <- sum(is.na(values))
    non_na_values <- values[!is.na(values)]
    
    tibble(
      total_rows = length(values),
      # Unique values (excluding NAs)
      unique_values = n_distinct(values, na.rm = TRUE),
      # NA statistics
      na_count = na_count,
      na_percentage = round(na_count / length(values) * 100, 2),
      # Duplicate statistics (including first occurrences)
      duplicate_rows = sum(dup_mask, na.rm = TRUE),
      duplicate_percentage = round(sum(dup_mask, na.rm = TRUE) / length(values) * 100, 2),
      # Most frequent value (excluding NAs)
      most_frequent_value = if(length(non_na_values) > 0) {
        as.character(names(sort(table(non_na_values), decreasing = TRUE)[1]))
      } else {
        "All NA"
      },
      most_frequent_count = if(length(non_na_values) > 0) {
        as.integer(max(table(non_na_values)))
      } else {
        0L
      },
      most_frequent_percentage = if(length(non_na_values) > 0) {
        round(max(table(non_na_values)) / length(non_na_values) * 100, 2)
      } else {
        0
      }
    )
  }, .id = "column") %>%  # This adds the column names
  arrange(desc(duplicate_rows), desc(na_count))


########## kpgz_db_katalog_dups_na
kpgz_db_katalog_dups_na <- kpgz_db_katalog %>%
  map_df(~ {
    values <- .
    dup_mask <- duplicated(values) | duplicated(values, fromLast = TRUE)
    na_count <- sum(is.na(values))
    non_na_values <- values[!is.na(values)]
    
    tibble(
      total_rows = length(values),
      # Unique values (excluding NAs)
      unique_values = n_distinct(values, na.rm = TRUE),
      # NA statistics
      na_count = na_count,
      na_percentage = round(na_count / length(values) * 100, 2),
      # Duplicate statistics (including first occurrences)
      duplicate_rows = sum(dup_mask, na.rm = TRUE),
      duplicate_percentage = round(sum(dup_mask, na.rm = TRUE) / length(values) * 100, 2),
      # Most frequent value (excluding NAs)
      most_frequent_value = if(length(non_na_values) > 0) {
        as.character(names(sort(table(non_na_values), decreasing = TRUE)[1]))
      } else {
        "All NA"
      },
      most_frequent_count = if(length(non_na_values) > 0) {
        as.integer(max(table(non_na_values)))
      } else {
        0L
      },
      most_frequent_percentage = if(length(non_na_values) > 0) {
        round(max(table(non_na_values)) / length(non_na_values) * 100, 2)
      } else {
        0
      }
    )
  }, .id = "column") %>%  # This adds the column names
  arrange(desc(duplicate_rows), desc(na_count))

########## tz_bktz_dups_na
tz_bktz_dups_na <- tz_bktz %>%
  map_df(~ {
    values <- .
    dup_mask <- duplicated(values) | duplicated(values, fromLast = TRUE)
    na_count <- sum(is.na(values))
    non_na_values <- values[!is.na(values)]
    
    tibble(
      total_rows = length(values),
      # Unique values (excluding NAs)
      unique_values = n_distinct(values, na.rm = TRUE),
      # NA statistics
      na_count = na_count,
      na_percentage = round(na_count / length(values) * 100, 2),
      # Duplicate statistics (including first occurrences)
      duplicate_rows = sum(dup_mask, na.rm = TRUE),
      duplicate_percentage = round(sum(dup_mask, na.rm = TRUE) / length(values) * 100, 2),
      # Most frequent value (excluding NAs)
      most_frequent_value = if(length(non_na_values) > 0) {
        as.character(names(sort(table(non_na_values), decreasing = TRUE)[1]))
      } else {
        "All NA"
      },
      most_frequent_count = if(length(non_na_values) > 0) {
        as.integer(max(table(non_na_values)))
      } else {
        0L
      },
      most_frequent_percentage = if(length(non_na_values) > 0) {
        round(max(table(non_na_values)) / length(non_na_values) * 100, 2)
      } else {
        0
      }
    )
  }, .id = "column") %>%  # This adds the column names
  arrange(desc(duplicate_rows), desc(na_count))

########## df_full
df_full_dups_na <- df_full %>%
  map_df(~ {
    values <- .
    dup_mask <- duplicated(values) | duplicated(values, fromLast = TRUE)
    na_count <- sum(is.na(values))
    non_na_values <- values[!is.na(values)]
    
    tibble(
      total_rows = length(values),
      # Unique values (excluding NAs)
      unique_values = n_distinct(values, na.rm = TRUE),
      # NA statistics
      na_count = na_count,
      na_percentage = round(na_count / length(values) * 100, 2),
      # Duplicate statistics (including first occurrences)
      duplicate_rows = sum(dup_mask, na.rm = TRUE),
      duplicate_percentage = round(sum(dup_mask, na.rm = TRUE) / length(values) * 100, 2),
      # Most frequent value (excluding NAs)
      most_frequent_value = if(length(non_na_values) > 0) {
        as.character(names(sort(table(non_na_values), decreasing = TRUE)[1]))
      } else {
        "All NA"
      },
      most_frequent_count = if(length(non_na_values) > 0) {
        as.integer(max(table(non_na_values)))
      } else {
        0L
      },
      most_frequent_percentage = if(length(non_na_values) > 0) {
        round(max(table(non_na_values)) / length(non_na_values) * 100, 2)
      } else {
        0
      }
    )
  }, .id = "column") %>%  # This adds the column names
  arrange(desc(duplicate_rows), desc(na_count))


suspic_data <- df_full %>% 
  filter(is.na(id_entity_kpgz_katalog_db_ktlg))





write_xlsx(susp, "C:/Users/GudievZK/Desktop/KatalogDB/IAMO_663/2025.09.30_kpgz_db_katalog_susp.xlsx")

