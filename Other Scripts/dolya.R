remove(list = ls())

rm(kpgz44_sum)

future_st_dubl <- 45336394784 + 9653914569
future_st <- 45336394784
now_st <- 1297853564

future_st_dubl_k <- 368
future_st_k <- 310
now_st_k <- 127

future_st/now_st
now_st/future_st*100

#### Step 1, 2 ####

# Из имеющегося перечня закупок исключены контракты, предметом которых является:
# 1) строительство, НИОКР, коммунальные услуги, КЖЦ, офсетные закупки.
# На данном этапе доля стандартизированных закупок по 44-ФЗ составила: 71,71%.
summ_all_step2 <- 683605430305.98 # 
# 2) 71,71% от 683605430305.98 составляет 490213454072
summ_st_step2 <- 683605430305.98/100*71.71
# 490213454072 от 683605430305.98 составляет 71,71% 
size_st_step2 <- summ_st_step2/summ_all_step2*100

# Чтобы 490213454072 млрд.руб (сумма стандартизации) составляла 75% процентов от объема регулярных, 
# необходимо, чтобы обьем регулярных закупок составлял 653,65 млрд.руб (490,2 млрд.руб/653,65 млрд.руб=75%)
summ_st_step2/653650000000

#  Для этого в свою очередь необходимо уменьшить сумму регулярных нестандартизированных закупок 
# с 193,3 млрд до 162,9 млрд руб (193,3 - 29,95= 163,4)
# Так как данная задача является весьма трудозатратной предлагаем повышать долю стандартизации путем исключения закупок 
# по позициям КПГЗ с использованием которых в 2020 или 2019 гг не было соверщено ни одной закупки (этап 3), а также увеличения 
#доли стандартизированных закупок за счет отнесения к последним позиций КПГЗ, в отношении которых разработаны проекты КТД 
# за текущий и предыдущие годы (этап 4).
summ_all_step2 - summ_st_step2 # 193 392 000 000


#### Step 3 ####
# Из имеющегося перечня закупок исключены контракты, заключенные с использованием нестандартизированных позиций КПГЗ, созданных не 
# позднее 19.08.2020, которые не использовались для проведения закупок в 2019 или 2020 году. Перечень таких КПГЗ приведен на листе 
# "Нерегулярные КПГЗ". Сумма нерегулярных закупок составила 12 782 663 231,51 рублей.
# На данном этапе доля стандартизированных закупок по 44-ФЗ составила: 73,08%.
iregular_kpgz <- 12782663231.51
summ_all_step3 <- summ_all_step2 - 12782663231.51
size_st_step3 <- summ_st_step2/summ_all_step3*100
#### Step 4 ####

getwd()
setwd('C:/Users/GudievZK/Desktop/GitHub/DF/')

df <- read.csv2("2022.05.13 size st.csv")
summary(df)
names(df)
str(df)

df$kpgz_sum <- as.numeric(df$kpgz_sum)
sum(df$kpgz_sum, na.rm = T)
View(df)


head(df$kpgz_sum)

sum(c(df$kpgz_sum, df$kpgz44_sum, df$kpgz223_sum))

future_kpgz <- subset(df, is_standard_product == 4 | is_standard_product == 5 | is_standard_product == 6 | is_standard_product == 11)
future_kpgz44_sum <- sum(future_kpgz$kpgz44_sum) # 44млрд 497млн 163тыс 021руб

now_kpgz <- subset(df, is_standard_product == 11)
now_kpgz_sum <- sum(now_kpgz$kpgz44_sum) # 3млрд 995млн 997тыс 072 руб

now_kpgz_sum/future_kpgz44_sum*100

gegular <- 944965376336.48 # сумма рег. закупок (44-фз, 223-фз) по файлу за прошлый год
persp_st <- 77712908530 # сумма перспективной стандартизации + 51 по файлу за прошлый год
sum_i_kv <- 5141713397  # сумма стандартизаированных в 1 квартале КПГЗ по за текущий год
sum_i_kv/persp_st*100 # за 1 квартал утвердилось 6.616293% от persp_st <- 77712908530

dolya_pers_st <- persp_st/gegular*100 # доля перспективной стандартизации в рег. закупках составляет 8.22389
sum_st_21 <- 73.31
dolya_pers_st + sum_st_21 # 81.53389 доля стандартизации с учетом перспективной стандартизации.

73.31+8.22389 # 73.31(c)

####  Сравнения расчетов за 21 год и 22 год ####
#
baza_21 <- 944965376336.48 # Объем регулярных закупок в 21 году по 44-ФЗ и 223_ФЗ = 944.965.376.336
baza_22 <- 996712017498.52 # Объем регулярных закупок в 22 году по 44-ФЗ и 223_ФЗ = 996.712.017.498

#
sumSt_21 <- 692759831252.16 # Сумма стандартизированных закупок в 21 году по 44-ФЗ и 223-ФЗ = 692.759.831.252
sumSt_22 <- 736904596172.16 # Сумма стандартизированных закупок в 22 году по 44-ФЗ и 223-ФЗ = 736.904.596.172

#
dolya_St21_otBaza21 <- sumSt_21/baza_21*100 # Доля станд. закупок по 44-ФЗ и 223-ФЗ от общего объема закупок по 44-ФЗ и 223-ФЗ в 21 году = 73.31
dolya_St22_otBaza22 <- sumSt_22/baza_22*100 # Доля станд. закупок по 44-ФЗ и 223-ФЗ от общего объема закупок по 44-ФЗ и 223-ФЗ в 22 году = 73.93


#
dolya_St21_otBaza22 <- sumSt_21/baza_22*100 # Сумма станд. закупок по 44-ФЗ и 223-ФЗ в 21 году от общего объема закупок по 44-ФЗ и 223-ФЗ в 22 году = 69.50%
dolya_St22_otBaza21 <- sumSt_22/baza_21*100 # Сумма станд. закупок по 44-ФЗ и 223-ФЗ в 22 году от общего объема закупок по 44-ФЗ и 223-ФЗ в 21 году = 77.98%


#### Расчеты по выгрузке за 22 год ####
# 44-ФЗ
sum_44_3step_22 <- 711256439821.45 # Сумма закупок по 44-ФЗ на 3 этапе = 711.256.439.821
sum_44_St_3step_22 <- 531636854246.26 # Сумма стандартизированных закупок по 44-ФЗ = 531.636.854.246
dolya_44_St_3step <- sum_44_St_3step_22/sum_44_3step_22*100 # доля станд. закупок по 44 закупок от общего объема закупок по 44-ФЗ на 3 этапе = 74.75%

# 223-ФЗ
sum_223_3step_22 <- 285270619441.56 # Сумма закупок по 223-ФЗ на 3 этапе = 285.270.619.441
sum_223_St_3step_22 <- 205267741925.90 # Сумма стандартизированных закупок по 223-ФЗ = 205.267.741.925
dolya_223_St_3step <- sum_223_St_3step_22/sum_223_3step_22*100 # Доля станд. закупок по 223 закупок от общего объема закупок по 223-ФЗ на 3 этапе = 71.96%

# 44-ФЗ + 223-ФЗ 
sum_3step_22 <- 996712017498.52 # Сумма закупок по 44-ФЗ и 223-ФЗ на 3 этапе = 996.712.017.498
sum_St_3step_22 <- 736904596172.16 # Сумма стандартизированных закупок по 44-ФЗ и 223-ФЗ = 736.904.596.172
dolya_St_3step <- sum_St_3step_22/sum_3step_22*100 # Доля станд. закупок по 44-ФЗ и 223-ФЗ от общего объема закупок по 44-ФЗ и 223-ФЗ на 3 этапе = 73.93%

katalog_all_date31.03.22 <- 170964

katalog_med_01.02.09_date31.03.22 <- 5230
katalog_med_01.02.10_date31.03.22 <- 82615
katalog_med_01.02.11_date31.03.22 <- 4864
katalog_med <- katalog_med_01.02.09_date31.03.22 + katalog_med_01.02.10_date31.03.22 + katalog_med_01.02.11_date31.03.22

katalog_exceptMed_date31.03.22 <- katalog_all_date31.03.22 - katalog_med

####  ####
zakupki <- read.csv2("2022.05.17 size st (mac).csv")
str(zakupki)
sum(zakupki$KPGZ_SUM) # 7 тлрн 082 млрд 731 млн 000 000 руб.
sum(zakupki$KPGZ_SUM[zakupki$status == 1]) # 688 млрд 780 млн 000 000 руб.
sum(zakupki$KPGZ_SUM[zakupki$status %in% c(1, 2, 3)]) # 736 млрд 904 млн 594 тыс 176 руб.
sort(zakupki)

# альтенативный способ
View(subset(zakupki, status %in% c(1, 2, 3)))
stand <- subset(zakupki, status %in% c(1, 2, 3))
sum(stand$KPGZ_SUM)
#


#### ggplot ####
names(zakupki)
str(zakupki)
zakupki_f_names <- c('isFinal_kpgz', 'status', 'IS_STANDARD_PRODUCT')
zakupki[, zakupki_f_names] <- lapply(zakupki[, zakupki_f_names], factor)


ggplot(zakupki, aes(x = 'status'))+
  geom_histogram(fill = "white", col = "black", binwidth = 2)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  ggtitle("MPG histogram")

ggplot(zakupki, aes(x = "KPGZ_SUM", fill = 'status'))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")


ggplot(zakupki, aes(x = KPGZ_SUM))+
  geom_density(fill = "red")

ggplot(zakupki, aes(x = KPGZ_SUM, fill = status))+
  geom_density(alpha = 0.5)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG density plot")


ggplot(zakupki, aes(x = status, y = KPGZ_SUM))+
  geom_boxplot()+
  xlab("Transmission type")+
  ylab("Gross horsepower")+
  scale_fill_discrete(name="Engine type")+
  ggtitle("Gross horsepower and engine type")


ggplot(df, aes(x = mpg, y = hp, size = qsec))+
  geom_point()+
  xlab("Miles/(US) gallon")+
  ylab("Gross horsepower")+
  scale_size_continuous(name="1/4 mile time")+
  ggtitle("Miles/(US) gallon and Gross horsepower")


my_plot  <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2  <- ggplot(df, aes(x = am, y = hp, fill = vs))

my_plot2 + geom_boxplot()


#### Анализ номинативных данных ####

yt <- yt %>%
  mutate(tru = case_when(
    startsWith(ktd, "П") ~ "Поставка товра",
    startsWith(ktd, "В") ~ "Выполнение работ",
    startsWith(ktd, "О") ~ "Оказание услуг",
  ))

zakupki <- zakupki %>%
  mutate(tru = case_when(
    startsWith(CODE, "01.") ~ "Поставка товра",
    startsWith(CODE, "02.") ~ "Выполнение работ",
    startsWith(CODE, "03.") ~ "Оказание услуг",
  ))


t1 <- table(zakupki$status)
t2 <- table(zakupki$status, zakupki$tru)
dim(zakupki)

prop.table(zakupki$status)

barplot(t2, fill = 'status', legend.text = T, args.legend = list(x = 'topright'))
barplot(t2, fill = 'status', legend.text = T, args.legend = list(x = 'topright'), beside = T)

mosaicplot(t2)

62000*107
6634000/24

ggplot(data = zakupki, aes(x = 'tru', y = 'Freq', fill = 'status')) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Brown", "Blue", "Darkgrey", "Darkgreen"))

install.packages("googlesheets4")
library(googlesheets4)




