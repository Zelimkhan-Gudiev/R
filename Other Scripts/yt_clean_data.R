# ����� ��������� ������� � RStudio ���� csv (��� ���������) ����������:
# 1.	��� �� Windows � ��� ���������� ���������� ��� ����� �CSV (����������� -�������)�
# 2.	��� �� Mac � ��� ���������� ���������� ��� ����� �CSV UTF-8 (����������� -�������)�
# 3. ����� ��������� ����������� ����� ��������� (� ����) ��������� File -> Reopen with Encoding -> Show all encoding -> CP1251

#### ����������� ������� ####

library(dplyr)
library(readxl)
library(xml2)
library(rvest)
library(htmlTable)
library(googlesheets4)
library(rvest)
library(writexl)
library(stringr)
library(tidyr)
library(tibble)

#### Setwd and clean data ####

yt <- read_html("http://youtrack-issues.bex.su/") %>% html_table(fill = T) %>% .[[1]] %>%
  select("ID ������", "���������", "���������", "���� �� ��������������", "�������", "��� ��", "Stage",
       "���������� ��������� �� �����", "���������� ��������� �� ���", "�����������", "������������ ������",
       "���������� ����������� ������������ ��", "����������� �����", "��������", "���", "�������� ������",
       "����� 2", "������� ����������� ���������� (����������, �����������)", "����", "���", "���� ��������",
       "����� ���������� � ������� \"� ������ ��\"", "����� ���������� � ������� \"��������� ���\"",
       "����� ���������� � ������� \"��������� �����\"", "����� ���������� � ������� \"����. ����.\"",
       "����� ���������� � ������� \"� �����\"", "����� ���������� � ������� \"�� ����. � ���\"", 
       "����� ���������� � ������� \"���������� � ��\"", "����� ���������� � ������� \"��\"", 
       "����� ���������� � ������� \"���\"", "����� ���������� � ������� \"�������� � �����\"",
       "������������") %>% 
   rename(id = "ID ������", name = "���������", reason = "���������", year_plan_st = "���� �� ��������������", 
       kvartal = "�������", kind_tz = "��� ��", stage = "Stage", numb_ret_depir = "���������� ��������� �� �����", 
       numb_ret_oiv = "���������� ��������� �� ���", executor_ac = "�����������", teamleader = "������������ ������",
       deputy = "���������� ����������� ������������ ��", executor_depir = "����������� �����", contract = "��������", 
       pcp = "���", criteria = "�������� ������", f2 = "����� 2", method = "������� ����������� ���������� (����������, �����������)", 
       tegs = "����", ktd = "���", created_date = "���� ��������", time_ac = "����� ���������� � ������� \"� ������ ��\"", 
       time_rev_oiv = "����� ���������� � ������� \"��������� ���\"", time_rev_depir = "����� ���������� � ������� \"��������� �����\"", 
       time_vn_sogl = "����� ���������� � ������� \"����. ����.\"", time_depir = "����� ���������� � ������� \"� �����\"", 
       time_oiv = "����� ���������� � ������� \"�� ����. � ���\"", time_prep_rg = "����� ���������� � ������� \"���������� � ��\"", 
       time_rg = "����� ���������� � ������� \"��\"", time_mrg = "����� ���������� � ������� \"���\"", 
       time_eaist = "����� ���������� � ������� \"�������� � �����\"", duration = "������������") %>% 
  slice(-c(1:3)) %>% 
  filter(!tegs %in% "�� ��������� YouTrack���" & stage %in% "���������")
  

# �������� ������ �������� � ������� ktd �� ����������� ���
yt$ktd[yt$ktd == ""] <- filter(yt, ktd == "")$name

# ��������� �������

yt <- yt %>% mutate(grade_duration = ifelse(duration > (mean(duration, na.rm = T) + sd(duration, na.rm = T)),
                               'Bad duration', "Good duration"),
             grade_numb_ret_depir = ifelse(numb_ret_depir > (mean(numb_ret_depir, na.rm = T) + sd(numb_ret_depir, na.rm = T)),
                                     "Bad numb_ret_depir", "Good numb_ret_depir"),
             grade_numb_ret_oiv = ifelse(numb_ret_oiv > (mean(numb_ret_oiv, na.rm = T) + sd(numb_ret_oiv)),
                                   "Bad numb_ret_oiv", "Good numb_ret_oiv"),
             tru = case_when(startsWith(ktd, "�") ~ "�������� �����",
                       startsWith(ktd, "�") ~ "���������� �����",
                       startsWith(ktd, "�") ~ "�������� �����"))

# ������ ��� ��������� ���������� �� ������

yt[, c('reason', 'year_plan_st', 'kvartal', 'kind_tz', 'executor_depir','stage', 'executor_ac', 
       'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
       'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
       'grade_numb_ret_oiv', 'tru')] <- lapply(yt[, c('reason', 'year_plan_st', 'kvartal', 'kind_tz', 'executor_depir','stage', 'executor_ac', 
                                                      'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
                                                      'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
                                                      'grade_numb_ret_oiv', 'tru')], as.factor)
# ������ ��� created_date �� date

yt$created_date <- str_extract_all(yt$created_date, "^.{10}") %>% unlist() # ������� ����� �� ���������� created_date

yt$created_date <- as.Date(yt$created_date)

# ������ ��� year_plan_st � �� Ordered factor
yt$year_plan_st <- ordered(yt$year_plan_st)

# ���������� ������ � ������������� �������

## �������� NA �� 0 � ���������� time_ac, time_rev_oiv, time_rev_depir
yt[, c("time_ac", "time_rev_depir", "time_rev_oiv")] <- replace_na(select(yt, time_ac, time_rev_depir, time_rev_oiv),
                                                                list(time_ac = 0, time_rev_depir = 0, time_rev_oiv = 0))
                                                      
## ���������� ���, � ������� ����� ���������� time_ac < 5 ��� time_rev_depir < 1 ��� time_rev_oiv < 1
suspicious_time <- filter(yt, time_ac <= 5 | time_rev_depir < 1 | time_rev_oiv < 3)

## ���������� ���, ������� �� ���� �� ������������ �� ��������� ����� ��� ��� 
suspicious_ret <- filter(yt, numb_ret_depir < 1 | numb_ret_oiv < 1)

## ��������� �������

suspicious_ktd <- rbind(suspicious_time, suspicious_ret)

## ��������� ������� ��� ����������

suspicious_ktd <- suspicious_ktd %>% 
  add_column(Comment_numb_ret_depir = ifelse(suspicious_ktd$numb_ret_depir == 0, 
                                             "����������� ��������� ��������� ����� ��� ���������������� ���������� ������ ���������� � YT", 
                                             "-"),
             .after = "numb_ret_depir"
             ) %>% 
  add_column(Comment_numb_ret_oiv = ifelse(suspicious_ktd$numb_ret_oiv == 0, 
                                           "����������� ��������� ��������� ��� ��� ���������������� ���������� ������ ���������� � YT", 
                                           "-"),
             .after = "numb_ret_oiv"
             ) %>% 
  add_column(Comment_time_ac = ifelse(suspicious_ktd$time_ac <= 5,
                                            "����������� ����� ���������� ��� ��� � ������ �������������� ����� ����������, ������� ������� ��-�� ������� ������ ���� � YT �����������", 
                                            "-"),
             .after = "time_ac"
             ) %>% 
  add_column(Comment_time_rev_depir = ifelse(suspicious_ktd$time_rev_depir < 1,
                                           "����������� ����� ��������� ��� �� ���������� ����� ��� � ������ �������������� ��������� ��� �� ���������� ���, ������� ������� ��-�� ������� ������ ���� � YT �����������", 
                                           "-"),
             .after = "time_rev_depir"
            ) %>% 
  add_column(Comment_time_rev_oiv = ifelse(suspicious_ktd$time_rev_oiv < 3,
                                           "����������� ����� ��������� ��� �� ���������� ��� ��� � ������ �������������� ��������� ��� �� ���������� ���, ������� ������� ��-�� ������� ������ ���� � YT �����������", 
                                           "-"),
             .after = "time_rev_oiv"
                              )

## ��������� �������������� ���
names(suspicious_ktd) %>% paste(sep = ", ")

suspicious_ktd %>% 
  select("id", "name", "reason", "year_plan_st", "kvartal", "kind_tz", "stage", "numb_ret_depir", "Comment_numb_ret_depir", "numb_ret_oiv", 
         "Comment_numb_ret_oiv", "executor_ac", "teamleader", "deputy", "executor_depir", "contract", "pcp", "criteria", "f2", "method", "tegs",
         "ktd", "created_date", "time_ac", "Comment_time_ac", "time_rev_oiv", "Comment_time_rev_oiv", "time_rev_depir", "Comment_time_rev_depir", 
         "time_vn_sogl", "time_depir", "time_oiv", "time_prep_rg", "time_rg", "time_mrg", "time_eaist", "duration", "tru") %>% 
  write_sheet(
    ss = gs4_get(
      "https://docs.google.com/spreadsheets/d/1ZZyeNdpO3IPbPXVgWt6rNMFgzRQPZ_LRhbW9Srq0QDg/edit#gid=0"
    ),
    sheet = "����1"
  )






filter(yt, is.na(yt$time_rev_oiv) & time_ac < 10)[["id"]] %>% 
  paste(collapse = ", ")

# ���������� ������������ ���� ����� � ��� �������

yt %>%
  write_sheet(
    ss = gs4_get(
      "https://docs.google.com/spreadsheets/d/1hXDJbsgdGZdjXxkxcaKKNJk9EwhP2CIPNaY5NilxkvY/edit?usp=sharing" # Replace the access link to the spreadsheets
    ),
    sheet = "list1"
  )

