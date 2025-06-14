 
library(readxl)
library(writexl)
library(dplyr)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(stringr)
library(purrr)

df <- read_xlsx("C:/Users/GudievZK/Downloads/������ C���_02_07_2024.xlsx", skip = 3)
df <- df[-1,]

colnames(df)
paste0(colnames(df), "'", ",", "'", collapse = "")

df <- fill(df,'������������� ����','������������ ����','����','����-2',
     '�����������������','����','������� ���������','�����','������',
     '���������','���� ���������� ���������','�������','���� ���')

df <- df %>% 
  filter(`������` == "����������" & `���������` == "��" & `�������` == "���")

df <- df %>% 
  filter(`��� ��������������` == "������������" & `��� �������������� ����` == "-") %>% distinct(`������������� ����`, .keep_all = T)
