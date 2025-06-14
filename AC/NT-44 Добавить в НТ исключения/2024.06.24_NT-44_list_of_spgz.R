library(readxl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)
library(rlang)
library(writexl)

spgz <- read_excel("C:/Users/GudievZK/Desktop/GitHub/AC/NT-44 �������� � �� ����������/2024.06.24 _NT-44_�������_���� � ������. � �.�. �����.xlsx", skip = 3)
spgz <- spgz[-1,]

colnames(spgz)
spgz <-fill(spgz, "������������� ����", "������������ ����", "����", "����-2", "�����������������", "����", 
                  "������� ���������", "�����", "������", "���������", "���� ���������� ���������", "�������", 
                  "���� ���")

spgz <- spgz %>% filter(spgz$`�������� ��������������` == "���������� ������� �������� �������������� ��� ������� ������ �����") %>% 
          distinct(`������������� ����`,.keep_all = T)


spgz <- filter(spgz, spgz$`������������ ��������������` %in% c("�������������� ��������������", "����������� ��������������",
                                                 "������������ ��������������", "���������������� ��������������") & 
        spgz$`�������� ��������������` == "���������� ������� �������� �������������� ��� ������� ������ �����") %>% 
        distinct(`������������� ����`,.keep_all = T)

table(as.factor(spgz$�����������������))

write_xlsx(spgz,"C:/Users/GudievZK/Desktop/GitHub/AC/NT-44 �������� � �� ����������/2024.06.24 _NT-44_spgz.xlsx")

