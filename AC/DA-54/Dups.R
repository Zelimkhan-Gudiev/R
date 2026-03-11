

df <- read.csv("C:/Users/GudievZK/Nextcloud/Data analysis/DA-54/2026.01.23_spgz_kpgz_okpd_ktru_packs.csv", fileEncoding = "Windows-1251") %>% as_tibble()
df_src <- read.csv("C:/Users/GudievZK/Nextcloud/Data analysis/DA-54/2026.01.23_spgz_kpgz_okpd_ktru_packs.csv", fileEncoding = "Windows-1251") %>% as_tibble()


df

df <- clean_names(df)
names(df)
dim(df)
glimpse(df)
str(df)
sapply(df, class)

sapply(df, function(x) sum(duplicated(x)))

df[duplicated(df$id_version_spgz_katalog) | duplicated(df$id_version_spgz_katalog, fromLast = TRUE), ]

dups <- df %>% 
  add_count(id_version_spgz_katalog, name = "n") %>% 
  filter(n > 1) # %>% 
  # sapply(function(x) sum(duplicated(x)))

dups %>% n_distinct()

sapply(dups, function(x) n_distinct(x))

sapply(dups, function(x) sum(duplicated(x)))

dups %>% group_by(id_version_spgz_katalog) %>% 
  filter(n() > 1) %>% 
  group_by(id_version_spgz_katalog, across(everything())) %>% 
  filter(n() > 1) %>% 
  ungroup()

dups_stat <- dups %>% 
  group_by(id_version_spgz_katalog) %>% 
  summarise(across(everything(), ~ n_distinct(.)), .groups = 'drop')

dups %>% 
  summarise(across(everything(),
                   list(
                     unique = ~ n_distinct(.),
                   #  duplicates = sum(duplicated(.)),
                     total = ~ n()
                    )))
  
  

  
# Get comprehensive statistics for each column within each group
summary_stats <- dups %>%
  group_by(id_version_spgz_katalog) %>%
  summarise(
    group_row_count = n(),  # Total rows in each group
    across(
      everything(),
      list(
        unique = ~ n_distinct(.),               # Number of unique values
        total = ~ n(),                           # Total non-NA values (same as group_row_count for each column)
        duplicated = ~ sum(duplicated(.) | duplicated(., fromLast = TRUE)),  # Total duplicated rows
        na_count = ~ sum(is.na(.))              # NA count
        # dup_rate = ~ 1 - (n_distinct(.) / n())   # Duplication rate
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = 'drop'
  )
