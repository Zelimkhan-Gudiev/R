install.packages("psych")
install.packages("lubridate")
library(psych)
library(googlesheets4)
library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(lubridate)

### 1. EDA
# read data
df_0 <- read_sheet("https://docs.google.com/spreadsheets/d/1esw9BCXZDO1h4hwuaKowia5eVmflEKTS6NiouamSYA0/edit?gid=723124268#gid=723124268", sheet = "Source_lettuce_dataset")

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
df_0$Date


  



### read data### readsum() data
df_0 <- read_sheet("https://docs.google.com/spreadsheets/d/1esw9BCXZDO1h4hwuaKowia5eVmflEKTS6NiouamSYA0/edit?gid=723124268#gid=723124268", sheet = "my_lettuce_dataset")



### procces colnames
df <- df %>% 
  select("plant_id":"growth_days")
colnames(df)
df <- clean_names(df)

### summary stats
summary(df)
summary(plant_summyry)

plant_summyry <- df %>% group_by(`plant_id`) %>% 
  summarise(temperature_c_mean = mean(temperature_c),
            humidity_percent_mean = mean(humidity_percent),
            tds_value_ppm_mean = mean(tds_value_ppm),
            p_h_level_mean = mean(p_h_level),
            growth_days_max = max(growth_days))

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

### procces colnames
df <- df %>% 
  select("plant_id":"growth_days")
colnames(df)
df <- clean_names(df)

### summary stats
summary(df)
summary(plant_summyry)

plant_summyry <- df %>% group_by(`plant_id`) %>% 
  summarise(temperature_c_mean = mean(temperature_c),
            humidity_percent_mean = mean(humidity_percent),
            tds_value_ppm_mean = mean(tds_value_ppm),
            p_h_level_mean = mean(p_h_level),
            growth_days_max = max(growth_days))

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
