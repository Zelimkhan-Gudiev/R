VIF <- function(test_data) {
  # Создаем копию набора данных, исключая первый столбец (предполагаемый отклик)
  test_df <- data.frame(test_data[, -1])
  
  # Создаем пустой вектор для хранения значений VIF
  VIF_df <- numeric(0)
  
  # Проходим по всем столбцам в наборе данных
  for (i in 1:ncol(test_df)) {
    # Строим линейную модель для текущего столбца, предсказывая его значения на основе остальных столбцов
    model <- lm(test_df[, i] ~ as.matrix(test_df[, -i]), data = test_df)
    
    # Вычисляем значение VIF для текущего столбца на основе коэффициента детерминации модели
    VIF_test <- 1 / (1 - summary(model)$r.squared)
    
    # Добавляем значение VIF в конец вектора VIF_df
    VIF_df <- c(VIF_df, VIF_test)
    
    # Задаем имя текущего столбца как имя соответствующего элемента вектора VIF_df
    names(VIF_df)[i] <- names(test_df)[i]
  }
  
  # Возвращаем вектор, содержащий значения VIF для каждого столбца в наборе данных
  return(VIF_df)
}
# VIF_df <- VIF(mtcars)

while (any(VIF_df > 10)) {
  i <- which.max(VIF_df)
  VIF_df <- VIF_df[- which.max(VIF_df)]
}
df <- mtcars[names(VIF_df)]
model <- lm(df[, 1] ~ ., df)
summary(model)$coef



