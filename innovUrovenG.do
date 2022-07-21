encode region, gen(region_)
xtset region_ year


***************************************************
* Визуальный анализ и анализ описательных статистик 
***************************************************


help spatwmat

spatwmat using wn_.dta, name(W) standardize eigenval(E)

net search spatwmat


xtsum lngrp linnexp  htech lnrd inn_gs res_num popul urban
kdensity grp 

kdensity htech
kdensity linnexp 
kdensity res_num
kdensity inn_gs 
kdensity lnrd 
kdensity urban 


kdensity popul


corr lngrp linnexp  htech lnrd inn_gs res_num popul urban


scatter lngrp linnexp 
scatter lngrp htech 
scatter lngrp lnrd 
scatter lngrp inn_gs
scatter lngrp res_num 
scatter lngrp popul
scatter lngrp urban




***************************************************
* Метод 0. Панель простая
***************************************************


xtreg lngrp linnexp  htech lnrd inn_gs res_num popul urban, fe vce(robust)
outreg2 using panel1.xls //Excel table

***************************************************
* Метод 1. Пространственная эконометрика 
***************************************************


*** Зададим кординаты для расчёта матрицы расстояний 
global xcoord latitude
global ycoord longitude
global band 25


spset region_

help spmat 
*** Расчитаем матрицу расчёта матрицы расстояний по ширине и долготе 
spatwmat, name(W) xcoord($xcoord) ycoord($ycoord) band(0 $band) standardize eigenval(E)


*** Диагностика пространственной структуры 

// расчёт статистики Морана
spatgsa lngrp linnexp  htech lnrd inn_gs res_num popul urban, w(W) moran

// построение диаграммы  Морана для lngrp
spatlsa lngrp , weights(W) moran graph(moran) symbol(id) id(region) 

// Тестирование пространственной корреляции ошибок
reg lngrp linnexp  htech lnrd inn_gs res_num popul urban
spatdiag, weights(W)

// Cтатистика Морана для остатков 
quiertly reg lngrp linnexp  htech lnrd inn_gs res_num popul urban
predict e_, residuals 
spatgsa e_, w(W) moran

// Пространственная коррелограмма 
spatcorr lngrp, bands(0(1)25) xcoord(longitude) ycoord(latitude) graph


*** Пространственная регрессия 
spatreg lngrp linnexp  htech lnrd inn_gs res_num popul urban, weights(W) eigenval(E) model(error)
outreg2 using srr.xls

spatreg lngrp linnexp  htech lnrd inn_gs res_num popul urban, weights(W) eigenval(E) model(lag) 
outreg2 using srr.xls



****Выбор модели по баесовским критериям 
spatreg lngrp linnexp  htech lnrd inn_gs res_num popul urban, weights(W) eigenval(E) model(error)
estat ic //AIC and BIC
spatreg lngrp linnexp  htech lnrd inn_gs res_num popul urban, weights(W) eigenval(E) model(lag)
estat ic //AIC and BIC




***************************************************
* Метод 2. 
***************************************************

help npregress

* Оценка непарметрическим методом local-constant и кросс-валидация 
npregress kernel lngrp linnexp  htech lnrd inn_gs res_num i.popul urban, vce(bootstrap, reps(100) seed(123)) estimator(constant)
outreg2 using up.xls //Excel table

* Оценка непарметрическим методом local-constant и AIC 
npregress kernel lngrp linnexp  htech lnrd inn_gs res_num i.popul urban, vce(bootstrap, reps(100) seed(123)) kernel(gaussian) imaic estimator(constant)
outreg2 using up.xls //Excel table


***Анализ предельных эффектов см. скрипт R





