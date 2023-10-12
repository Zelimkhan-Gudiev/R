library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(writexl)
library(xlsx)
library(stringr)

##### Read list of tz #####
# ktd <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 tz.xls")
ktd <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 tz.xls")
colnames(ktd) <- c("name", "kpgz", "status_of_tz", "stz", "Mandatory_usage", "date_of_last_change_of_the_tz", "date_of_approval_of_the_tz")
ktd <- fill(ktd, c("name", "status_of_tz", "stz", "Mandatory_usage", "date_of_last_change_of_the_tz", "date_of_approval_of_the_tz"))

##### Read list of kpgz #####
# kpgz <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>% 
kpgz <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>%
  filter(!row_number() %in% 1)
colnames(kpgz) <- c("id_kpgz", "kpgz", "okpd2", "is_standartised", 
                    "ktru", "package", "status_of_kpgz", "is_actual", 
                    "date_of_last_change_of_the_kpgz", "is_deleted")

##### Join dataframes #####  
ktd <- left_join(ktd, kpgz, by = c("kpgz"))

##### Processing of data ##### 
ktd <- ktd %>% select("name", "kpgz", "okpd2")
ktd <- separate(ktd, col = "kpgz", sep = "^\\S*\\K\\s+", into = c("kpgz_code", "kpgz_name"))
ktd <- separate(ktd, col = "okpd2", sep = "^\\S*\\K\\s+", into = c("okpd2_code", "okpd2_name"))
# ktd <- separate(ktd, col = "ktru", sep = "^\\S*\\K\\s+", into = c("ktru_code", "ktru_name"))



##### Read list of olpd #####
okpd <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.10 okpd.xlsx")
colnames(okpd) <- c("N", "okpd2_code", "okpd2_name")



### Create new columns with information about okpd2
okpd$okpd2_class_code <- str_extract(okpd$okpd2_code, "..")
okpd <- okpd[, c(4, 2, 3)]

ktd$okpd2_class_code <- strtrim(ktd$okpd2_code, 2)

filter(okpd, nchar(okpd$okpd2_code) <= 2)

ktd_2 <- left_join(x = ktd, y = filter(okpd, nchar(okpd$okpd2_code) <= 2), by = "okpd2_class_code")
ktd_2 <- select(ktd_2, name, kpgz_code, kpgz_name, 
       okpd2_code = okpd2_code.x, 
       okpd2_name = okpd2_name.x, 
       okpd2_class_code, 
       okpd2_class_name = okpd2_name.y)


# ktd$okpd2_section_code
# ktd$okpd2_section_name




# write_xlsx(ktd, "/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/2023.10.08 ktd.xlsx")
write_xlsx(ktd_2, "C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/ktd.xlsx")
