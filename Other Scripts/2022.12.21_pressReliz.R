library(tidyverse)
library(readxl)
library(dplyr)
library("writexl")
library(tidyr)

df9 <- read_excel("C:/Users/GudievZK/Downloads/2021.12.22 press.xlsx", sheet = "9month", )
df11 <- read_excel("C:/Users/GudievZK/Downloads/2021.12.22 press.xlsx", sheet = "11month")
f_names <- c("nameTz", "low", "status")
df9[f_names] <- lapply(df9[f_names], factor)
df9$`dateKontract` <- as.Date(df9$`dateKontract`)
str(df9)
df11[f_names] <- lapply(df11[f_names], factor)
df11$`dateKontract` <- as.Date(df11$`dateKontract`)
str(df11)
rm(f_names)

sum(is.na(df9$nameTz))
sum(is.na(df11$nameTz))

df9 <- mutate(df9, month = format(dateKontract, "%m"))
df11 <- mutate(df11, month = format(dateKontract, "%m"))


########

length(df9$status)                                # Количество закупок за 9 месяцев - 50473
sum(df9$cost)                                     # Cумма закупок за 9 месяцев - 484 292 622 504
length(df9$status[!is.na(df9$nameTz)])            # Количество закупок за 9 месяцев без NA - 50429 (50429 - 50473 = - 44)
sum(df9$cost[!is.na(df9$nameTz)])                 # Cумма закупок за 9 месяцев без NA - 482 251 617 609 (482251617609 - 484292622504 = - 2 041 004 895)

table(df9$nameTz) %>% sort(, decreasing = T, ) %>% .[1:3] # Топ 3 КТД за 9 месяцев:
                                                                 # 1) Поставка лекарственных средств - 13087
                                                                 # 2) Поставка хозяйственных товаров - 3114
                                                                 # 3) Поставка канцелярских товаров, бумаги и бумажных изделий - 1905


## Расчеты с учетом NA
df9 %>% group_by(month = lubridate::floor_date(dateKontract, 'month')) %>%
        summarize(sum = sum(cost))
aggregate(cost ~ month, df9, sum)
                                                ## Суммы закупок за 9 месяцев с использованием ТД с разбивкой по месяцам
                                                # month               sum
                                                # <date>            <dbl>
                                                # 1 2022-01-01 44247798726.
                                                # 2 2022-02-01 30031348976.
                                                # 3 2022-03-01 62586733964.
                                                # 4 2022-04-01 59221836550.
                                                # 5 2022-05-01 63293442845.
                                                # 6 2022-06-01 68371244069.
                                                # 7 2022-07-01 68778364764.
                                                # 8 2022-08-01 45546682389.
                                                # 9 2022-09-01 42215170220.

df9 %>% group_by(month = lubridate::floor_date(dateKontract, 'month')) %>%
        summarize(length = length(cost))
aggregate(cost ~ month, df9, length)
                                                    ## Количество зза 9 месяцев с использованием ТД с разбивкой по месяцам
                                                    #    month      length
                                                    #    <date>      <int>
                                                    # 1 2022-01-01   4558
                                                    # 2 2022-02-01   4089
                                                    # 3 2022-03-01   5694
                                                    # 4 2022-04-01   7088
                                                    # 5 2022-05-01   6318
                                                    # 6 2022-06-01   5893
                                                    # 7 2022-07-01   4859
                                                    # 8 2022-08-01   6183
                                                    # 9 2022-09-01   5791

## Расчеты без учета NA

filter(df9, !is.na(df9$nameTz))  %>% group_by(month = lubridate::floor_date(dateKontract, 'month')) %>%
                                     summarize(sum = sum(cost))
aggregate(cost ~ month, filter(df9, !is.na(df9$nameTz)), sum)
                                                  ## Количество зза 9 месяцев без учета NA с использованием ТД с разбивкой по месяцам
                                                  # month               sum
                                                  # <date>            <dbl>
                                                  # 1 2022-01-01 43922768303.
                                                  # 2 2022-02-01 29202747884.
                                                  # 3 2022-03-01 62582041415.
                                                  # 4 2022-04-01 59199635035.
                                                  # 5 2022-05-01 63237333674.
                                                  # 6 2022-06-01 68368774246.
                                                  # 7 2022-07-01 68395930471.
                                                  # 8 2022-08-01 45179713290.
                                                  # 9 2022-09-01 42162673289.

filter(df9, !is.na(df9$nameTz))  %>% group_by(month = lubridate::floor_date(dateKontract, 'month')) %>%
                                     summarize(length = length(cost))
aggregate(cost ~ month, filter(df9, !is.na(df9$nameTz)), length)
                                                  
                                                  ## Сумма за 9 месяцев без учета NA с использованием ТД с разбивкой по месяцам
                                                  # month      length
                                                  # <date>      <int>
                                                  # 1 2022-01-01   4547
                                                  # 2 2022-02-01   4082
                                                  # 3 2022-03-01   5691
                                                  # 4 2022-04-01   7085
                                                  # 5 2022-05-01   6315
                                                  # 6 2022-06-01   5891
                                                  # 7 2022-07-01   4851
                                                  # 8 2022-08-01   6181
                                                  # 9 2022-09-01   5786


topCount <- df9 %>%
            group_by(month, nameTz) %>%
            summarise(cnt = n()) %>%
            ungroup() %>%
            group_by(month) %>%
            top_n(3, cnt)


topSum <- df9 %>%
          group_by(month, nameTz) %>%
          summarize(cnt = n(), sumCost = sum(cost)) %>%
          ungroup() %>%
          group_by(month) %>%
          top_n(3, sumCost)

table(df9$nameTz) %>% sort(, decreasing = T, ) %>% .[1:3] # Топ 3 КТД за 9 месяцев:
                                                          # 1) Поставка лекарственных средств - 13087
                                                          # 2) Поставка хозяйственных товаров - 3114
                                                          # 3) Поставка канцелярских товаров, бумаги и бумажных изделий - 1905

############ Расчеты на основании выгрузки за 12-й месяц
aggregate(cost ~ month, filter(df11, !is.na(df11$nameTz)), length)
aggregate(cost ~ month, filter(df11, !is.na(df11$nameTz)), sum)

############ Расчеты на основании выгрузки за 11 месяцев
# df11 <- filter(df11, month != 12)
aggregate(cost ~ month, filter(df11, !is.na(df11$nameTz) & month != 12), sum)
aggregate(cost ~ month, filter(df11, !is.na(df11$nameTz) & month != 12), length)


filter(df11, month != 12) %>% length(.$status)


length(df11$status)                                # Количество закупок за 11 месяцев - 68924
sum(df11$cost)                                     # Cумма закупок за 11 месяцев - 628 320 465 943
length(df11$status[!is.na(df11$nameTz)])           # Количество закупок за 11 месяцев без NA - 68837 (68837 - 68924 = - 87)
sum(df11$cost[!is.na(df11$nameTz)])                # Cумма закупок за 11 месяцев без NA - 623 254 664 799 (623254664799 - 628320465943 = - 5 065 801 144)

table(df11$nameTz) %>% sort(, decreasing = T, ) %>% .[1:3] # Топ 3 КТД за 11 месяцев:
                                                           # 1) Поставка лекарственных средств - 13087
                                                           # 2) Поставка хозяйственных товаров - 3114
                                                           # 3) Поставка канцелярских товаров, бумаги и бумажных изделий - 1905


