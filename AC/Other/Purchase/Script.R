library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

getwd()
list.files()

#### Read list of tz
# tz <- read_excel("C:/Users/GudievZK/Desktop/SpinData/2023.12.06 ������ �������� ��.xls")
tz <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/Purchase/2023.12.06 tz.xlsx")
# tz <- fill(tz, "������������", "������", "��������� ��", "�������������� �������������", "���� ���������� ���������", "���� �����������")
# write_xlsx(tz, "C:/Users/GudievZK/Desktop/SpinData/2023.12.06 tz.xls")

tz <- separate(tz, ����, into = c("��� ����", "������������ ����"), sep = "^\\S*\\K\\s+")

#### Read list of relations
# relations <- read_excel("C:/Users/GudievZK/Desktop/SpinData/2023.12.06 ������ ����������� ������.xlsx")

relations <- read_excel("/Users/zelimkhan/Desktop/Data/GitHub/Purchase/2023.12.06 ������ ����������� ������??.xlsx")

relations <- separate(relations, "����������� ���� (��������� �������)", 
                      into = c("��� ����������� ���� (��������� �������)", 
                                                                   "������������ ����������� ���� (��������� �������)"),
                      sep = "^\\S*\\K\\s+")


#### join dataframes
relations <- left_join(relations, tz, by = c("��� ����������� ���� (��������� �������)" = "��� ����"))


# write_xlsx(relations, "C:/Users/GudievZK/Desktop/SpinData/2023.12.06 relations.xlsx")

write_xlsx(relations, "/Users/zelimkhan/Desktop/Data/GitHub/Purchase/relations.xlsx")

relations %>% 
  filter(!is.na(������������))

relations_eq <- relations %>% 
  filter(`��� �����` == "=", !is.na(������������), 
         `��������� ��` == "���")


relations_eq <- relations_eq %>% 
  select("������������� � �� ��������...1", 
         "������������� ������ � �� ��������...2",
         "������������� � �����...3",
         "������������� ������ � �����...4",
         "������� ���������...12",
         "���� �������� �������...22",
         "��� � ������������ ��������� ����/������������ ��������� ����"
         
         "��� �����",
         
         "������������� � �� ��������...30",
         "������������� ������ � �� ��������...31",
         "������������� � �����...32",
         "������������� ������ � �����...33",
         "��� � ������������/������������ ��������� ����/����",
         "������� ���������...41",
         "������� ��� ��� ������...39",
         "������� ������...42",
         "���� �������� �������...49",
         "���� �������� �������...51",
         "������������",
         "���� �����������")
  
names(relations_eq)













