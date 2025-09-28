install.packages("psych")
install.packages("lubridate")
install.packages("purrr")
install.packages("GGally")
library(psych)
library(googlesheets4)
library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)
library(purrr)
library(GGally)


### 1. EDA
# read data
df_0 <- read_sheet("https://docs.google.com/spreadsheets/d/1esw9BCXZDO1h4hwuaKowia5eVmflEKTS6NiouamSYA0/edit?gid=723124268#gid=723124268", sheet = "Source_lettuce_dataset")
df_0
### 1.1. Understand the dataset
### 1.1.1. What variables (columns) exist?
colnames(df_0)
### 1.1.2. What types of data do they contain (numeric, categorical, dates)?
str(df_0)
### 1.1.3. How many rows are there?
dim(df_0)


### 1.2. Check data quality
### 1.2.1. Are there missing values?
colSums(is.na(df_0))
### 1.2.2. Are there any duplicates or errors?
sapply(df_0, function(x) sum(duplicated(x)))
### 1.2.3. Are values in a reasonable range (e.g. temperature not 500 C)?
summary(df_0)

df_0 <- df_0 %>%
  mutate(
    # 1) Flatten: make every element a clean character string
    Date = map_chr(Date, ~ if (inherits(.x, "POSIXt"))
      format(.x, "%Y-%m-%d") else as.character(.x)),
    Date = trimws(Date),
    Date = gsub("[\u2010\u2011\u2012\u2013\u2014\u2212]", "-", Date),  # normalize dashes
    Date = na_if(Date, ""),                                          # treat empty as NA
    
    # 2) Parse: try ISO (Y-m-d) then US (m/d/Y); keep first non-NA
    Date = coalesce(
      ydm(Date, tz = "UTC", quiet = TRUE),
      mdy(Date, tz = "UTC", quiet = TRUE)
    )
  ) %>% arrange(Date)
df_0 <- df_0 %>% 
  mutate(Date = as_date(Date))


summary(df_0$Date)


### 1.3 Summarize statistics
### 1.3.1 Mean, median, min, max, variance, percentiles

summary(df_0)

# Transform data to perform aggregation and calculation

plant_summyry <- df %>% group_by(`plant_id`) %>% 
  summarise(temperature_c_min = min(temperature_c),
            temperature_c_mean = mean(temperature_c),
            temperature_c_median = median(temperature_c),
            temperature_c_sd = sd(temperature_c),
            temperature_c_max = max(temperature_c),
            humidity_percent_min = min(humidity_percent),
            humidity_percent_mean = mean(humidity_percent),
            humidity_percent_median = median(humidity_percent),
            humidity_percent_sd = sd(humidity_percent),
            humidity_percent_max = max(humidity_percent),
            tds_value_ppm_min = min(tds_value_ppm),
            tds_value_ppm_mean = mean(tds_value_ppm),
            tds_value_ppm_median = median(tds_value_ppm),
            tds_value_ppm_sd = sd(tds_value_ppm),
            tds_value_ppm_max = max(tds_value_ppm),
            p_h_level_min = min(p_h_level),
            p_h_level_mean = mean(p_h_level),
            p_h_level_median = median(p_h_level),
            p_h_level_sd = sd(p_h_level),
            p_h_level_max = max(p_h_level),
            growth_days_min = min(growth_days),
            growth_days_mean = mean(growth_days),
            growth_days_median = median(growth_days),
            growth_days_sd = sd(growth_days),
            growth_days_max = max(growth_days))


plant_summyry[, 2:26] <- apply(select(plant_summyry, -plant_id), 2, round, digits = 1)

summary(plant_summyry)

plant_sum_by_id <- plant_summyry %>% 
  select(plant_id, temperature_c_mean,
         humidity_percent_mean,
         tds_value_ppm_mean,
         p_h_level_mean,
         growth_days_max)
plant_sum_by_id$plant_id <- as.numeric(plant_sum_by_id$plant_id)
plant_sum_by_id <- apply(plant_sum_by_id, 2, round, digits = 1) %>% as_tibble()



plant_sum_by_growth_days_max <- df %>% 
  group_by(plant_id) %>% 
  mutate(growth_days_max = max(growth_days)) %>% 
  group_by(growth_days_max) %>% 
  summarise(plant_id_n = n_distinct(plant_id),
            temperature_c_mean = mean(temperature_c),
            humidity_percent_mean = mean(humidity_percent),
            tds_value_ppm_mean = mean(tds_value_ppm),
            p_h_level_mean = mean(p_h_level)
            )

plant_sum_by_growth_days_max <- apply(plant_sum_by_growth_days_max, 2, round, digits = 1) %>% as_tibble()

### 1.3.2 Counts and frequencies for categorical data.
###

### 1.4 Visualize relationships

### 1.4.1 Histograms (distribution).
df %>% select(where(is.numeric)) %>% 
  pivot_longer(everything(),
               names_to = "variable", values_to = "value") %>% 
  ggplot(aes(value, fill = variable)) +
  geom_histogram(bins = 30, alpha = 0.6, color = "black", na.rm = TRUE) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(x = NULL, y = "Count") +
#  theme_minimal() + 
  guides(fill = "none")


### 1.4.3 Boxplots (outliers).

df %>% select(where(is.numeric)) %>% 
  pivot_longer(everything(),
               names_to = "variable", values_to = "value") %>% 
  ggplot(aes(x = variable, y = value)) + 
  geom_boxplot(outlier.alpha = 0.35) + 
  labs(x = NULL, y = NULL, title = "Boxplots of numeric variables") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
 # theme_minimal() + 
  guides(fill = "none")
  

### 1.4.4  Time-series plots (trends over time).

df %>% 
  pivot_longer(-c(date, plant_id), names_to = "variable", values_to = "values") %>% 
  group_by(variable, date) %>% 
  summarise(mean = mean(values)) %>% 
  arrange(variable, date) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line() + 
  facet_wrap(~ variable, scales = "free_y", ncol = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = NULL, y = NULL, title = "Daily mean") + 
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())


df %>%
  pivot_longer(-c(date, plant_id), names_to = "variable", values_to = "value") %>% 
  group_by(variable, date) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    p25  = quantile(value, 0.25, na.rm = TRUE),
    p75  = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(variable, date) %>% 
  ggplot(aes(date, mean)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = NULL, y = NULL, title = "Daily mean with IQR band") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())


### 1.4.5 Scatterplots (check correlation).

df %>% select(where(is.numeric)) %>% 
  select(- plant_id) %>% 
  ggpairs(progress = FALSE,
          upper = list(continuous = wrap("cor", method = "pearson")),
          diag = list(continuous = "densityDiag"),
          lwer = list(continuous = wrap("points", alpha = 0.4, size = 0.7))
    
  )

### 1.5 Form hypotheses

### 1.5.1 “Does higher humidity improve growth?”

### 1.5.2 “Are weekly growth patterns consistent?”

### 1.5.3 “Is there seasonality in the data?”











### correlation analysis
cor(plant_sum_by_growth_days_max)
cor(plant_sum_by_id)

# create plots to visualize correlations
cor.plot(
  select(df, "temperature_c":"growth_days")
)
cor.plot(
  select(plant_summyry, "temperature_c_mean":"growth_days_max")
)

cor.plot(
  cor(
    select(plant_summyry,"temperature_c_mean":"growth_days_max")
  )
)

### disrtrubations
hist(df$temperature_c)

df_sub <- select(df, "temperature_c":"growth_days")

df_long <- df_sub %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


ggplot(df_long, aes(x = value, fill = variable))+
  geom_histogram(bins = 30, alpha = 0.6, color = "black")+
  facet_wrap(~ variable, scales = "free")+
  theme_minimal()

df <- df %>% group_by(plant_id) %>% 
  mutate(growth_days_max = max(growth_days))

growth_days_df<- as.factor(df$growth_days_max) %>% table() %>% as.data.frame()
colnames(growth_days_df) <- c("growth_days_df_max", "frequency")

ggplot(growth_days_df, aes(x = growth_days_df_max))+
  geom_histogram(bins = 30)
hist(growth_days_df$frequency)

### procces colnames
df <- df %>% 
  select("plant_id":"growth_days")
colnames(df)
df <- clean_names(df)

### summary stats
summary(df)
summary(plant_summyry)



### correlation analysis
cor(select(df, "temperature_c":"growth_days"))
cor(plant_summyry)

# create plots to visualize correlations
cor.plot(
  select(df, "temperature_c":"growth_days")
  )
cor.plot(
  select(plant_summyry, "temperature_c_mean":"growth_days_max")
  )

cor.plot(
  cor(
    select(plant_summyry,"temperature_c_mean":"growth_days_max")
    )
  )

### disrtrubations
hist(df$temperature_c)

df_sub <- select(df, "temperature_c":"growth_days")

df_long <- df_sub %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


ggplot(df_long, aes(x = value, fill = variable))+
  geom_histogram(bins = 30, alpha = 0.6, color = "black")+
  facet_wrap(~ variable, scales = "free")+
  theme_minimal()


df <- df %>% group_by(plant_id) %>% 
  mutate(growth_days_max = max(growth_days))

growth_days_df<- as.factor(df$growth_days_max) %>% table() %>% as.data.frame()
colnames(growth_days_df) <- c("growth_days_df_max", "frequency")

ggplot(growth_days_df, aes(x = growth_days_df_max))+
  geom_histogram(bins = 30)
hist(growth_days_df$frequency)
