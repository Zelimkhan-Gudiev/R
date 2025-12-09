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
library(skimr)
library(writexl)

df_0 <- read_excel("C:/Users/GudievZK/Nextcloud/Data analysis/DA-53/2025.12.08 Обход стандартизации.xlsx")
df_0 <- clean_names(df)

names(df_0)
str(df_0)
glimpse(df_0)

sapply(df_0, class)
colSums(is.na(df_0))
sapply(df_0, function(x) sum(duplicated(x)))
sapply(df_0, function(x) sum(n_distinct(x)))

df_0 <- df_0 %>% mutate(
  data_publikacii_izvesenia_v_eis = as_date(data_publikacii_izvesenia_v_eis, origin = "1899-12-30"),
  data_okoncania_podaci_zaavok = as_date(data_okoncania_podaci_zaavok, origin = "1899-12-30"),
  data_standartizacii = as_date(data_standartizacii, origin = "1899-12-30")
)

df_0 <- df_0 %>% mutate(
  kompleks = as.factor(kompleks),
  grbs = as.factor(grbs),
  sposob_opredelenia_postavsika_podradnoj_organizacii = as.factor(sposob_opredelenia_postavsika_podradnoj_organizacii),
  naimenovanie_ispol_zovannogo_ktd = as.factor(naimenovanie_ispol_zovannogo_ktd)
)

df_0 <- df_0 %>% 
  mutate(
    is_standart_purchase = as.factor(ifelse(naimenovanie_ispol_zovannogo_ktd == "-", "No", "Yes"))
  )

names(df_0)
toString(names(df_0))

df <- df_0 %>% 
  distinct(reestrovyj_nomer_lota, nomer_procedury_v_eaist, reestrovyj_nomer_zakupki_v_eis, .keep_all = TRUE) %>% 
  filter(is_standart_purchase == "No") %>% 
  select(reestrovyj_nomer_lota, nomer_procedury_v_eaist, 
         reestrovyj_nomer_zakupki_v_eis, naimenovanie_predmeta_zakupki, 
         kompleks, grbs, zakazcik, data_publikacii_izvesenia_v_eis, 
         data_okoncania_podaci_zaavok,
         sposob_opredelenia_postavsika_podradnoj_organizacii, 
         nacal_naa_maksimal_naa_cena_rub,
         is_standart_purchase)

summary(df)
skim(df)

toString(names(df_0))

write_xlsx(df, "C:/Users/GudievZK/Nextcloud/Data analysis/DA-53/2025.12.08_df_non_standart.xlsx")
write_xlsx(df_0, "C:/Users/GudievZK/Nextcloud/Data analysis/DA-53/2025.12.08_df_0.xlsx")
