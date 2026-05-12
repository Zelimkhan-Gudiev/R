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

df_src <- read_xlsx("C:/Users/GudievZK/Nextcloud/Data analysis/DA-68/2026.04.13_Детализация по лотам 2025_итог2.xlsx", sheet = "2025")
names(df_src)
df_src <- clean_names(df_src) 
str(df_src)
glimpse(df_src)
describe(df_src)
skim(df_src)

summary(df_src)

sapply(df_src, function(x) sum(duplicated(x)))

sapply(df_src, function(x) n_distinct(x))

sapply(df_src, function(x) sum(is.na(x)))
 
  
stats <- df_src %>% 
    summarise(
    across(
      .cols = everything(),
      .fns = list(
        non_na = ~ sum(!is.na(.)),
        na = ~ sum(is.na(.)),
        duplicates = ~ sum(duplicated(.) & !is.na(.)),
        unique = ~ n_distinct(., na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>% pivot_longer(
          cols = everything(),
          names_to = c("column", "metric"),
          names_sep = "__",
          values_to = "value"
  ) %>% 
    pivot_wider(
      names_from = metric,
      values_from = value
  )


unique(df$sposob_opredelenia_postavsika)

df %>% filter(sposob_opredelenia_postavsika == "Закупка у единственного поставщика")
