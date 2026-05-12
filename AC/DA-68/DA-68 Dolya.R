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

df_src <- read_xlsx("C:/Users/GudievZK/Nextcloud/Data analysis/DA-68/2026.03.31 АМиПМ.xlsx")
names(df)
df <- clean_names(df_src) 
str(df)
glimpse(df)
describe(df)
skim(df)

summary(df)

sapply(df, function(x) sum(duplicated(x)))

sapply(df, function(x) n_distinct(x))
 
  
stats <- df %>% 
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
