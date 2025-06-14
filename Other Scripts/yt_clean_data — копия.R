#### ����������� ������� ####

library(dplyr)
library(readxl)
library(xml2)
library(rvest)
library(htmlTable)
library(googlesheets4)
library(rvest)
library(writexl)

yt <- as_sheets_id("1hpTRLxNeXfvjfnFhnI4hOM8L6o8s45bxLeY94NO-1jQ") %>% read_sheet(sheet = "2023.02.18 yt") %>% 
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
  filter(!tegs %in% "�� ��������� YouTrack���" & stage %in% "���������") %>% 
  mutate(grade_duration = ifelse(duration > (mean(duration, na.rm = T) + sd(duration, na.rm = T)),
                                 'Bad duration', "Good duration"),
         grade_numb_ret_depir = ifelse(numb_ret_depir > (mean(numb_ret_depir, na.rm = T) + sd(numb_ret_depir, na.rm = T)),
                                       "Bad numb_ret_depir", "Good numb_ret_depir"),
         grade_numb_ret_oiv = ifelse(numb_ret_oiv > (mean(numb_ret_oiv, na.rm = T) + sd(numb_ret_oiv)),
                                     "Bad numb_ret_oiv", "Good numb_ret_oiv"),
         tru = case_when(startsWith(ktd, "�") ~ "�������� �����",
                         startsWith(ktd, "�") ~ "���������� �����",
                         startsWith(ktd, "�") ~ "�������� �����"))

yt[, c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 
       'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
       'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
       'grade_numb_ret_oiv')] <- lapply(yt[, c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 
                                               'teamleader', 'deputy', 'contract', 'pcp', 'criteria', 'f2', 
                                               'method', 'tegs', 'grade_duration', 'grade_numb_ret_depir', 
                                               'grade_numb_ret_oiv')], as.factor)
