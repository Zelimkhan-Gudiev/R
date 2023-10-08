library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(writexl)
library("xlsx")

ktd <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 tz.xls")
colnames(ktd) <- c("name", "kpgz", "status_of_tz", "stz", "Mandatory_usage", "date_of_last_change_of_the_tz", "date_of_approval_of_the_tz")
ktd <- fill(ktd, c("name", "status_of_tz", "stz", "Mandatory_usage", "date_of_last_change_of_the_tz", "date_of_approval_of_the_tz"))

kpgz <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>% 
  filter(!row_number() %in% 1)
colnames(kpgz) <- c("id_kpgz", "kpgz", "okpd2", "is_standartised", 
                    "ktru", "package", "status_of_kpgz", "is_actual", 
                    "date_of_last_change_of_the_kpgz", "is_deleted")
  
ktd <- left_join(ktd, kpgz, by = c("kpgz"))

ktd <- ktd %>% select("name", "stz", "date_of_approval_of_the_tz", 
                      "id_kpgz", "kpgz", "okpd2", "ktru")
ktd <- separate(ktd, col = "kpgz", sep = "^\\S*\\K\\s+", into = c("kpgz_code", "kpgz_name"))
ktd <- separate(ktd, col = "okpd2", sep = "^\\S*\\K\\s+", into = c("okpd2_code", "okpd2_name"))
ktd <- separate(ktd, col = "ktru", sep = "^\\S*\\K\\s+", into = c("ktru_code", "ktru_name"))

write_xlsx(ktd, "/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/2023.10.08 ktd.xlsx")
