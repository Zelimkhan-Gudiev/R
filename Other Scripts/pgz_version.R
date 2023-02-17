library(readxl)
library(dplyr)
install.packages("writexl")
library("writexl")

df <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/DF/df_pgz_plusVersion.xlsx")

colnames(df)


filter(df, Id_entity == 59351)


df %>% group_by(Id_entity, code_name) %>% 
  summarise(count = length(code_name)) %>% 
  arrange(Id_entity) %>% 
  filter(duplicated(Id_entity)) %>% 
  .[["Id_entity"]] -> search_id

df_name_changed <- filter(df, Id_entity %in% search_id) %>% arrange(Id_entity) %>% arrange(Id_entity,  desc(data_approval))
df_name_changed %>% unique(Id_entity) %>% length()
length(unique(df_name_changed$Id_entity))
getwd()

write_xlsx(df_name_changed, "/Users/zelimkhan/Desktop/Data/GitHub/DF/df_name_changed.xlsx")
