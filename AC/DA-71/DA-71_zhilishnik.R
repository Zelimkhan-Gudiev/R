library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(psych)
library(stringr)
library(psych)
library(googlesheets4)
library(tidyr)
library(janitor)
library(ggplot2)
library(purrr)
library(GGally)
library(skimr)
library(writexl)
library(corrplot)

df_src <- read_excel("/Users/zelimkhan/Desktop/DA-XX_zhilishnik/df_zhilishnik_contract_spec_2025.xlsx", sheet = "src")

df_src <- clean_names(df_src)
toString(names(df_src))
names(df_src)
df <- df_src %>% 
  rename(
    naimenovanie_predmet_kontrakta = naimenovanie_predmet_gk, 
    osnovanie_zaklucenia_ed_post = osnovanie_zaklucenia, 
    status_kontrakta = status_gk, 
    cena_kontrakta_rub = cena_gk_rub,
    data_zaklucenia_kontrakta = data_zaklucenia_gk, 
    zakon_osnovanie_kontrakta = zakon_osnovanie_kontrakty,
    spgz_cena_rub = cena_rub, 
    spgz_kol_vo = kol_vo, 
    summa_spgz_rub = summa_pozicii_rub,
    spgz_ed_izm = ed_izm
  )

df <- df %>%
  mutate(
    tru = substr(kpgz_konecnyj_kod, 1, 2) # Creates a new column with the first 2 characters
  )

df <- df %>% 
  select(spgz, spgz_ed_izm, spgz_cena_rub, spgz_kol_vo, summa_spgz_rub,
         tru, kpgz_konecnyj_kod, kpgz_konecnyj_naimenovanie,
         reestr_nomer_kontrakta_v_eis, reestrovyj_nomer_izvesenia_eis, reestrovyj_nomer_v_rk,
         naimenovanie_predmet_kontrakta, cena_kontrakta_rub, 
         zakon_osnovanie_kontrakta, osnovanie_zaklucenia_ed_post, status_kontrakta, 
         zakazcik, grbs, data_zaklucenia_kontrakta, postavsik,
         kod_okpd2, okpd2)



df <- df %>% # Takes the existing data frame and saves the changed version back into df
  mutate(
    across(
      c(tru, zakazcik, zakon_osnovanie_kontrakta, 
        osnovanie_zaklucenia_ed_post, status_kontrakta, grbs), # Selects columns that should be converted
      as.factor                                                # Converts each selected column to factor
    )
  )

skim(df)
str(df)


df %>% 
  group_by(tru) %>% 
  summarise(unique_kontrakts = n_distinct(reestr_nomer_kontrakta_v_eis),
            unique_izvesenia = n_distinct(reestrovyj_nomer_izvesenia_eis),
            unique_reestrovyj_nomer_v_rk = n_distinct(reestrovyj_nomer_v_rk),
            unique_kpgz = n_distinct(kpgz_konecnyj_kod),
            unique_spgz = n_distinct(spgz))

            

df_stat <- df %>% filter(spgz_ed_izm != "Условная единица" & spgz_ed_izm != "Условная единица" & tru != "02" & !is.na(reestr_nomer_kontrakta_v_eis)) %>% 
  group_by(reestrovyj_nomer_v_rk, spgz) %>% 
  mutate(
    unique_reestrovyj_nomer_v_rk = n_distinct(reestrovyj_nomer_v_rk),
    min_cost_spgz = min(spgz_cena_rub),
    max_cost_spgz = max(spgz_cena_rub),
    mean_cost_spgz = mean(spgz_cena_rub),
    sd_cost_spgz = sd(spgz_cena_rub))
            
###
df_vithout_reestr_n_kontracta <- df %>% filter(reestr_nomer_kontrakta_v_eis == "-") %>% 
  select(spgz, spgz_ed_izm, spgz_cena_rub, spgz_kol_vo, summa_spgz_rub,
         tru, kpgz_konecnyj_kod, kpgz_konecnyj_naimenovanie, reestrovyj_nomer_v_rk,
         reestr_nomer_kontrakta_v_eis, reestrovyj_nomer_izvesenia_eis,
         zakazcik, naimenovanie_predmet_kontrakta, cena_kontrakta_rub, 
         zakon_osnovanie_kontrakta, osnovanie_zaklucenia_ed_post, status_kontrakta, 
         grbs, data_zaklucenia_kontrakta, postavsik,
         kod_okpd2, okpd2)

df_vithout_reestr_n_kontracta %>% skim()

unique(df_vithout_reestr_n_kontracta$osnovanie_zaklucenia_ed_post)
n_distinct(df_vithout_reestr_n_kontracta$osnovanie_zaklucenia_ed_post)
table(df_vithout_reestr_n_kontracta$osnovanie_zaklucenia_ed_post)              
