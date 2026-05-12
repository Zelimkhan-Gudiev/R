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

#### SPGZ
spgz_src <- read_excel("C:/Users/GudievZK/Nextcloud/Data analysis/DA-66/2026.03.10_spgz.xlsx", skip = 3) %>% slice(-1)

names(spgz_src)
spgz_src <- clean_names(spgz_src)
str(spgz_src)


skim(spgz_src)

toString(names(spgz_src))

spgz <- spgz_src %>% fill(identifikator_spgz, naimenovanie_spgz, kpgz, 
                      okpd_2, standartizirovana, ktru, 
                      edinicy_izmerenia, paket, status, 
                      aktual_na, data_poslednego_izmenenia, 
                      udalena, est_pcp, .direction = "down")



# Update the spgz data frame and save the result back into spgz              # The full pipeline modifies spgz step by step
spgz <- spgz %>%
  mutate(
    medicine = if_else(                                                      # Create the medicine column based on the prefix in kpgz
      startsWith(as.character(kpgz), "01.02"),                               # Check whether values in kpgz start with "01.02"
      "Да",                                                                  # If the condition is TRUE, assign "Да"
      "Нет"                                                                  # If the condition is FALSE, assign "Нет"
    ),
    spgz_ktru = if_else(                                                     # Create the spgz_ktru column based on the value in ktru
      ktru == "-",                                                           # Check whether ktru is exactly "-"
      "Нет",                                                                 # If TRUE, assign "Нет"
      "Да"                                                                   # If FALSE, assign "Да"
    ),
    characteristic_ktru = if_else(                                           # Create the characteristic_ktru column based on kod_harakteristiki_ktru
      kod_harakteristiki_ktru == "-",                                        # Check whether kod_harakteristiki_ktru is exactly "-"
      "Нет",                                                                 # If TRUE, assign "Нет"
      "Да"                                                                   # If FALSE, assign "Да"
    )
  ) %>%
  group_by(identifikator_spgz) %>%                                           # Group rows with the same identifikator_spgz value
  mutate(
    has_add_chars = if_else(                                                 # Create a new column that shows whether the group has additional characteristics
      n_distinct(characteristic_ktru, na.rm = TRUE) > 1,                     # Count distinct non-missing characteristic_ktru values inside each group and check whether there is more than one
      "Да",                                                                  # If there is more than one unique value, assign "Да"
      "Нет"                                                                  # Otherwise, assign "Нет"
    )
  ) %>%
  ungroup() %>%                                                              # Remove grouping so later operations work on the whole data frame normally
  mutate(
    across(
      .cols = where(is.character),                                           # Select all character columns in the data frame
      .fns = ~ if (length(unique(.[!is.na(.)])) < 10) factor(.) else .       # Convert character columns with fewer than 10 unique non-missing values to factors
    )
  )


summary(spgz)
describe(spgz)
skim(spgz)

spgz <- spgz %>% 
  select(identifikator_spgz, spgz_ktru, naimenovanie_spgz, 
         kpgz, standartizirovana, medicine, has_add_chars,
         naimenovanie_harakteristiki, characteristic_ktru,
         znacenie_harakteristiki, tip_harakteristiki, 
         tip_vybora_znacenij_harakteristiki_zakazcikom)

spgz %>% group_by(medicine, has_add_chars, standartizirovana, spgz_ktru) %>% 
  summarise(n_spgz = n_distinct(identifikator_spgz)) # %>% 
#  pull(n_spgz) %>%                                                           # Extract the numeric summary column as a vector
#  sum()        


spgz %>% group_by(medicine, has_add_chars, standartizirovana, spgz_ktru) %>% 
  summarise(n_spgz = n_distinct(identifikator_spgz),
            n_chars = n())

# Group rows and calculate summary statistics for each group
stats_spgz <- spgz %>% 
  group_by(medicine, has_add_chars, standartizirovana, spgz_ktru) %>% 
  summarise(
    n_spgz = n_distinct(identifikator_spgz),            # Count distinct SPGZ identifiers in the group
    n_kpgz = n_distinct(kpgz),
    n_chars = n(),                                      # Count rows (total characteristics)
    n_add_chars = sum(characteristic_ktru == "Нет"),    # Count rows where characteristic_ktru equals "Нет"
    .groups = "drop"                                    # Remove grouping after summarisation
  )

#### KPGZ

kpgz_src <- read_excel("C:/Users/GudievZK/Nextcloud/Data analysis/DA-66/2026.03.10_kpgz.xlsx", skip = 3) %>% slice(-1)

names(kpgz_src)
kpgz_src <- clean_names(kpgz_src)
str(kpgz_src)


toString(names(kpgz_src))

kpgz <- kpgz_src %>% fill(identifikator_kpgz, kpgz, okpd_2, standartizirovana, 
                          ktru, paket, status, aktual_na, data_poslednego_izmenenia, 
                          udalena,
                          .direction = "down")




# Update the spgz data frame and save the result back into spgz              # The full pipeline modifies spgz step by step
kpgz <- kpgz %>%
  mutate(
    medicine = if_else(                                                      # Create the medicine column based on the prefix in kpgz
      startsWith(as.character(kpgz), "01.02"),                               # Check whether values in kpgz start with "01.02"
      "Да",                                                                  # If the condition is TRUE, assign "Да"
      "Нет"                                                                  # If the condition is FALSE, assign "Нет"
    ),
    kpgz_ktru = if_else(                                                     # Create the spgz_ktru column based on the value in ktru
      ktru == "-",                                                           # Check whether ktru is exactly "-"
      "Нет",                                                                 # If TRUE, assign "Нет"
      "Да"                                                                   # If FALSE, assign "Да"
    ),
    characteristic_ktru = if_else(                                           # Create the characteristic_ktru column based on kod_harakteristiki_ktru
      kod_harakteristiki_ktru == "-",                                        # Check whether kod_harakteristiki_ktru is exactly "-"
      "Нет",                                                                 # If TRUE, assign "Нет"
      "Да"                                                                   # If FALSE, assign "Да"
    )
  ) %>%
  group_by(identifikator_kpgz) %>%                                           # Group rows with the same identifikator_spgz value
  mutate(
    has_add_chars = if_else(                                                 # Create a new column that shows whether the group has additional characteristics
      n_distinct(characteristic_ktru, na.rm = TRUE) > 1,                     # Count distinct non-missing characteristic_ktru values inside each group and check whether there is more than one
      "Да",                                                                  # If there is more than one unique value, assign "Да"
      "Нет"                                                                  # Otherwise, assign "Нет"
    )
  ) %>%
  ungroup() %>%                                                              # Remove grouping so later operations work on the whole data frame normally
  mutate(
    across(
      .cols = where(is.character),                                           # Select all character columns in the data frame
      .fns = ~ if (length(unique(.[!is.na(.)])) < 10) factor(.) else .       # Convert character columns with fewer than 10 unique non-missing values to factors
    )
  )


summary(kpgz)
describe(kpgz)
skim(kpgz)

kpgz <- kpgz %>% 
  select(identifikator_kpgz, kpgz_ktru, naimenovanie_kpgz, 
         kpgz, standartizirovana, medicine, has_add_chars,
         naimenovanie_harakteristiki, characteristic_ktru,
         znacenie_harakteristiki, tip_harakteristiki, 
         tip_vybora_znacenij_harakteristiki_zakazcikom)

kpgz %>% group_by(medicine, has_add_chars, standartizirovana, kpgz_ktru) %>% 
  summarise(n_spgz = n_distinct(identifikator_spgz)) # %>% 
#  pull(n_spgz) %>%                                                           # Extract the numeric summary column as a vector
#  sum()        


kpgz %>% group_by(medicine, has_add_chars, standartizirovana, kpgz_ktru) %>% 
  summarise(n_kpgz = n_distinct(identifikator_kpgz),
            n_chars = n())

# Group rows and calculate summary statistics for each group
stats_kpgz <- kpgz %>% 
  group_by(medicine, has_add_chars, standartizirovana, kpgz_ktru) %>% 
  summarise(
    n_kpgz_id = n_distinct(identifikator_kpgz),            # Count distinct SPGZ identifiers in the group
    n_kpgz_name = n_distinct(kpgz),
    n_chars = n(),                                      # Count rows (total characteristics)
    n_add_chars = sum(characteristic_ktru == "Нет"),    # Count rows where characteristic_ktru equals "Нет"
    .groups = "drop"                                    # Remove grouping after summarisation
  )

### Writing xlsx

stats <- list("stats_kpgz" = stats_kpgz, "stats_spgz" = stats_spgz)


kpgz %>% 
  filter(has_add_chars == "Да") %>% 
  group_by(medicine) %>% 
  summarise(
    n_kpgz_id = n_distinct(identifikator_kpgz),            # Count distinct SPGZ identifiers in the group
    n_kpgz_name = n_distinct(kpgz),
    n_chars = n(),                                      # Count rows (total characteristics)
    n_add_chars = sum(characteristic_ktru == "Нет"),    # Count rows where characteristic_ktru equals "Нет"
    .groups = "drop"                                    # Remove grouping after summarisation
  )


write_xlsx(stats, "C:/Users/GudievZK/Nextcloud/Data analysis/DA-66/2026.03.11_stats_df.xlsx")
