library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(stringr)
library(writexl)
library(stringr)

##### Read list of tz #####
ktd <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 tz.xls")
# ktd <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 tz.xls")
colnames(ktd) <- c("name", "kpgz", "status_of_tz", "stz", "Mandatory_usage", "date_of_last_change_of_the_tz", "date_of_approval_of_the_tz")
ktd <- fill(ktd, c("name", "status_of_tz", "stz", "Mandatory_usage", "date_of_last_change_of_the_tz", "date_of_approval_of_the_tz"))
ktd <- separate(ktd, col = "kpgz", sep = "^\\S*\\K\\s+", into = c("kpgz_code", "kpgz_name"))
ktd <- ktd %>% select("name", "kpgz_code", "kpgz_name")

##### Read list of kpgz #####
kpgz <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>% 
# kpgz <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>%
  filter(!row_number() %in% 1)
colnames(kpgz) <- c("id_kpgz", "kpgz", "okpd2", "is_standartised", 
                    "ktru", "package", "status_of_kpgz", "is_actual", 
                    "date_of_last_change_of_the_kpgz", "is_deleted")
kpgz <- separate(kpgz, col = "kpgz", sep = "^\\S*\\K\\s+", into = c("kpgz_code", "kpgz_name"))
kpgz <- separate(kpgz, col = "okpd2", sep = "^\\S*\\K\\s+", into = c("okpd2_code", "okpd2_name"))
kpgz <- kpgz %>% select(kpgz_code, kpgz_name, okpd2_code, okpd2_name)


##### Join dataframes ktd & kpgz #####  
ktd <- left_join(ktd, select(kpgz, !kpgz_name), by = "kpgz_code")
ktd$okpd2_class_code <- str_extract(ktd$okpd2_code, "..")


##### Read list of okpd #####
okpd <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.10 okpd.xlsx")
# okpd <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.10 okpd.xlsx")
colnames(okpd) <- c("N", "okpd2_code", "okpd2_name")

### Create new columns with information about okpd2
okpd$okpd2_class_code <- str_extract(okpd$okpd2_code, "..")
okpd <- okpd[, c(4, 2, 3)]

##### Join dataframes ktd & okpd #####  
ktd <- left_join(x = ktd, y = filter(okpd, nchar(okpd$okpd2_code) <= 2), by = "okpd2_class_code")

ktd <- select(ktd, name, kpgz_code, kpgz_name, 
                okpd2_code = okpd2_code.x, 
                okpd2_name = okpd2_name.x, 
                okpd2_class_code, 
                okpd2_class_name = okpd2_name.y)

#### Creating okpd2_section_code & okpd2_section_name columns ####
ktd <- ktd %>% 
  mutate(okpd2_section_code = case_when(
           str_starts(okpd2_code, "01|02|03") ~ "������ A", 
           str_starts(okpd2_code, "05|06|07|08|09") ~ "������ B",
           str_starts(okpd2_code, "10|11|12|13|14|15|
                                              |16|17|18|19|20|21|22|23|24|
                                              |25|26|27|28|29|30|31|32|33") ~ "������ C",
           str_starts(okpd2_code, "35") ~ "������ D",
           str_starts(okpd2_code, "36|37|38|39") ~ "������ E",
           str_starts(okpd2_code, "41|42|43") ~ "������ F",
           str_starts(okpd2_code, "45|46|47") ~ "������ G",
           str_starts(okpd2_code, "49|50|51|52|53") ~ "������ H",
           str_starts(okpd2_code, "55|56") ~ "������ I",
           str_starts(okpd2_code, "58|59|60|61|62|63") ~ "������ J",
           str_starts(okpd2_code, "^64|^65|^66") ~ "������ K",
           str_starts(okpd2_code, "68") ~ "������ L",
           str_starts(okpd2_code, "69|70|71|72|73|74|75") ~ "������ M",
           str_starts(okpd2_code, "77|78|79|80|81|82") ~ "������ N",
           str_starts(okpd2_code, "85") ~ "������ P",
           str_starts(okpd2_code, "86|87|88") ~ "������ Q",
           str_starts(okpd2_code, "90|91|92|93") ~ "������ R",
           str_starts(okpd2_code, "94|95|96") ~ "������ S"),
         
         okpd2_section_name = case_when(
           str_starts(okpd2_code, "01|02|03") ~ "��������� ���������, ������� � ������� ���������",
           str_starts(okpd2_code, "05|06|07|08|09") ~ "��������� ��������������� �����������",
           str_starts(okpd2_code, "10|11|12|13|14|15|
                                              |16|17|18|19|20|21|22|23|24|
                                              |25|26|27|28|29|30|31|32|33") ~ "��������� �������������� �����������",
           str_starts(okpd2_code, "35") ~ "��������������, ���, ��� � ����������������� �������",
           str_starts(okpd2_code, "36|37|38|39") ~ "�������������; �������������, ������ �� �������� � ������������� �������",
           str_starts(okpd2_code, "41|42|43") ~ "���������� � ������������ ������",
           str_starts(okpd2_code, "45|46|47") ~ "������ �� ������� � ��������� ��������; ������ �� ������� ���������������� ������� � ����������",
           str_starts(okpd2_code, "49|50|51|52|53") ~ "������ ���������� � ���������� ���������",
           str_starts(okpd2_code, "55|56") ~ "������ ������������ ��������� � ������������� �������",
           str_starts(okpd2_code, "58|59|60|61|62|^63") ~ "������ � ������� ���������� � �����",
           str_starts(okpd2_code, "^64|^65|^66") ~ "������ ���������� � ���������",
           str_starts(okpd2_code, "68") ~ "������, ��������� � ���������� ����������",
           str_starts(okpd2_code, "69|70|71|72|73|74|75") ~ "������, ��������� � �������, ���������-����������� � ���������������� �������������",
           str_starts(okpd2_code, "77|78|79|80|81|82") ~ "������ ���������������� � ���������������",
           str_starts(okpd2_code, "85") ~ "������ � ������� �����������",
           str_starts(okpd2_code, "86|87|88") ~ "������ � ������� ��������������� � ���������� ������",
           str_starts(okpd2_code, "90|91|92|93") ~ "������ � ������� ���������, �����������, ������ � ������",
           str_starts(okpd2_code, "94|95|96") ~ "������ ������",
         ))

# a<- c(94:96)
# substr(paste0("^", a, "|", collapse = ""), 1, nchar(paste0("^", a, "|", collapse = ""))-1)


#### Creating kpgz_code_4digits_code & kpgz_code_4digits_name columns ####

ktd$kpgz_code_4digits_code <- str_extract(ktd$kpgz_code, ".....")
ktd <- left_join(ktd, subset(kpgz, nchar(kpgz$kpgz_code) == 5, select = c(kpgz_code, kpgz_name)), 
                   by = c("kpgz_code_4digits_code" = "kpgz_code"))
ktd <- rename(ktd, kpgz_code_4digits_name = kpgz_name.y,
              kpgz_name = kpgz_name.x)

ktd <- select(ktd, name, okpd2_section_code, okpd2_section_name,
                         okpd2_class_code, okpd2_class_name,
                         okpd2_code, okpd2_name,
                         kpgz_code_4digits_code, kpgz_code_4digits_name,
                         kpgz_code, kpgz_name)

# write_xlsx(ktd, "C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/ktd.xlsx")
write_xlsx(ktd, "/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/ktd.xlsx")



ktd %>% 
  group_by(kpgz_code_4digits_name) %>% 
  summarise(count_tz = length(unique(name))) %>% arrange(desc(count_tz))



write_xlsx(df, "/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/df.xlsx")

df_d <- df[duplicated(df$name), ]
