library(readxl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)
library(rlang)

df <- read_excel("C:/Users/GudievZK/Desktop/GitHub/DF/AC/2024.06.19_DA-22__MKR_Îò÷åò ïî çàêóïêàì çà 2023.xlsx")

colnames(df) <- c("kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date",
                  "using_tz", "notice_publication_date", "subject_of_procurement", "contract_date", "lot_id",
                  "contract_eis_registry_number", "contract_completion_date", "law", "method_of_determinig_supplier",
                  "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
                  "sum_of_contracts")
df <- df[-c(1, 2),]

df <- fill(df, "kod_kpgz", "name_kpgz", "is_standart", "grbs", "customer", "supplier", "name_tz", "tz_approval_date",
           "using_tz", "notice_publication_date", "subject_of_procurement", "contract_date",                            ##"lot_id",
           "contract_eis_registry_number", "contract_completion_date", "law", "method_of_determinig_supplier",
           "Basis_for_concluding_a_contract_with_a_single_supplier", "object_status", "number_of_contracts",
           "sum_of_contracts")
df_temp <- filter(df, !str_detect(df$customer, "^ÃÁ|^\"ÃÁ|^ÃÊ|^ÀÎ|^ÃÀÓ|^ÃÓÏ|^ÃÀÏÎÓ|^ÃÀÎÓ|^ÎÎÎ|^«ÍÈÈ|^ÌÀÎÓ|
                                  |^ÑÑ è ÍÌÏ|^ÎÀÎ|^«Ìîé îñîáûé ñåìåéíûé|^ÊÏ|^ÁŞĞÎ|^ÃÏÁÓ|^ÌÁÓ|^ÌÀÄÎÓ|
                                  |^ÌÀÓ|^ÌÓ |^ÌÓÊ |^ÌÎÑÊÎÂÑÊÈÉ ÃÎÑÓÄÀĞÑÒÂÅÍÍÛÉ ÎÁÚÅÄÈÍÅÍÍÛÉ ÌÓÇÅÉ-ÇÀÏÎÂÅÄÍÈÊ|
                                  |^Ôîíä ğåíîâàöèè|^ÌÃÓÓ Ïğàâèòåëüñòâà Ìîñêâû, Óíèâåğñèòåò Ïğàâèòåëüñòâà Ìîñêâû|
                                  |^Ìîñêîâñêèé ôîíä çàùèòû ïğàâ äîëüùèêîâ|^ÖÏÊèÎ èì. Ì. Ãîğüêîãî|
                                  |^«Ìîñêîâñêèé òåàòğ «Ñîâğåìåííèê»"))



df1 <- df_temp %>% filter(str_detect(df_temp$subject_of_procurement, "äëÿ íóæä"))
