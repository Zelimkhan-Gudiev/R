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
ktd <- separate(ktd, col = "kpgz", sep = "^\\S*\\K\\s+", into = c("kpgz_code", "kpgz_name"))
ktd <- ktd %>% select("name", "kpgz_code", "kpgz_name")

##### Read list of kpgz #####
# kpgz <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>% 
kpgz <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.08 kpgz.xlsx", skip = 3) %>%
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
okpd <- read_excel("C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/Source documentation/2023.10.10 okpd.xlsx")
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
           str_starts(okpd2_code, "01|02|03") ~ "пюгдек A", 
           str_starts(okpd2_code, "05|06|07|08|09") ~ "пюгдек B",
           str_starts(okpd2_code, "10|11|12|13|14|15|
                                              16|17|18|19|20|21|22|23|24|
                                              25|26|27|28|29|30|31|32|33") ~ "пюгдек C",
           str_starts(okpd2_code, "35") ~ "пюгдек D",
           str_starts(okpd2_code, "36|37|38|39") ~ "пюгдек E",
           str_starts(okpd2_code, "41|42|43") ~ "пюгдек F",
           str_starts(okpd2_code, "45|46|47") ~ "пюгдек G",
           str_starts(okpd2_code, "49|50|51|52|53") ~ "пюгдек H",
           str_starts(okpd2_code, "55|56") ~ "пюгдек I",
           str_starts(okpd2_code, "58|59|60|61|62|63") ~ "пюгдек J",
           str_starts(okpd2_code, "^64|^65|^66") ~ "пюгдек K",
           str_starts(okpd2_code, "68") ~ "пюгдек L",
           str_starts(okpd2_code, "69|70|71|72|73|74|75") ~ "пюгдек M",
           str_starts(okpd2_code, "77|78|79|80|81|82") ~ "пюгдек N",
           str_starts(okpd2_code, "85") ~ "пюгдек P",
           str_starts(okpd2_code, "86|87|88") ~ "пюгдек Q",
           str_starts(okpd2_code, "90|91|92|93") ~ "пюгдек R",
           str_starts(okpd2_code, "94|95|96") ~ "пюгдек S"),
         
         okpd2_section_name = case_when(
           str_starts(okpd2_code, "01|02|03") ~ "опндсйжхъ яекэяйнцн, кеямнцн х пшамнцн унгъиярбю",
           str_starts(okpd2_code, "05|06|07|08|09") ~ "опндсйжхъ цнпмнднашбючыху опнхгбндярб",
           str_starts(okpd2_code, "10|11|12|13|14|15|
                                              16|17|18|19|20|21|22|23|24|
                                              25|26|27|28|29|30|31|32|33") ~ "опндсйжхъ напюаюршбючыху опнхгбндярб",
           str_starts(okpd2_code, "35") ~ "щкейрпнщмепцхъ, цюг, оюп х йнмдхжхнмхпнбюмхе бнгдсую",
           str_starts(okpd2_code, "36|37|38|39") ~ "бнднямюафемхе; бндннрбедемхе, сяксцх он сдюкемхч х пейскэрхбюжхх нрунднб",
           str_starts(okpd2_code, "41|42|43") ~ "яннпсфемхъ х ярпнхрекэмше пюанрш",
           str_starts(okpd2_code, "45|46|47") ~ "сяксцх он норнбни х пнгмхвмни рнпцнбке; сяксцх он пелнмрс юбрнрпюмяонпрмшу япедярб х лнрнжхйкнб",
           str_starts(okpd2_code, "49|50|51|52|53") ~ "сяксцх рпюмяонпрю х яйкюдяйнцн унгъиярбю",
           str_starts(okpd2_code, "55|56") ~ "сяксцх цнярхмхвмнцн унгъиярбю х наыеярбеммнцн охрюмхъ",
           str_starts(okpd2_code, "58|59|60|61|62|^63") ~ "сяксцх б накюярх хмтнплюжхх х ябъгх",
           str_starts(okpd2_code, "^64|^65|^66") ~ "сяксцх тхмюмянбше х ярпюунбше",
           str_starts(okpd2_code, "68") ~ "сяксцх, ябъгюммше я медбхфхлшл хлсыеярбнл",
           str_starts(okpd2_code, "69|70|71|72|73|74|75") ~ "сяксцх, ябъгюммше я мюсвмни, хмфемепмн-реумхвеяйни х опнтеяяхнмюкэмни деърекэмнярэч",
           str_starts(okpd2_code, "77|78|79|80|81|82") ~ "сяксцх юдлхмхярпюрхбмше х бяонлнцюрекэмше",
           str_starts(okpd2_code, "85") ~ "сяксцх б накюярх напюгнбюмхъ",
           str_starts(okpd2_code, "86|87|88") ~ "сяксцх б накюярх гдпюбннупюмемхъ х янжхюкэмше сяксцх",
           str_starts(okpd2_code, "90|91|92|93") ~ "сяксцх б накюярх хяйсяярбю, пюгбкевемхи, нрдшую х яонпрю",
           str_starts(okpd2_code, "94|95|96") ~ "опнвхе сяксцх",
         ))

# a<- c(94:96)
# substr(paste0("^", a, "|", collapse = ""), 1, nchar(paste0("^", a, "|", collapse = ""))-1)

#### Creating kpgz_code_4digits_code & kpgz_code_4digits_name columns ####

ktd$kpgz_code_4digits_code <- str_extract(ktd$kpgz_code, ".....")
ktd <- left_join(ktd, subset(kpgz, nchar(kpgz$kpgz_code) == 5, select = c(kpgz_code, kpgz_name)), 
                   by = c("kpgz_code_4digits_code" = "kpgz_code"))

write_xlsx(ktd, "C:/Users/GudievZK/Desktop/GitHub/2023.10.08 KTD classifacation/ktd.xlsx")





