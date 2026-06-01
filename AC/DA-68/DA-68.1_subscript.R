procedure_level <- df %>%
  group_by(zakon_osnovanie, reestrovyj_nomer_procedury_eaist) %>%
  summarise(
    # Берём первую непустую НМЦК процедуры, потому что НМЦК повторяется по строкам спецификации.
    nmc_summa_po_vsej_procedure = first(na.omit(nmc_summa_po_vsej_procedure)),
    
    # Считаем сумму строк спецификации внутри процедуры.
    specification_sum = sum(summa_lota_po_pozicii_kpgz_rub, na.rm = TRUE),
    
    # Считаем сумму стандартизированных строк спецификации.
    standardized_specification_sum = sum(
      if_else(priznak_standartizacii_da_net == "да", summa_lota_po_pozicii_kpgz_rub, 0),
      na.rm = TRUE
    ),
    
    # Считаем количество строк спецификации в процедуре.
    rows_n = n(),
    
    # Считаем количество стандартизированных строк.
    standardized_rows_n = sum(priznak_standartizacii_da_net == "да", na.rm = TRUE),
    
    # Считаем количество нестандартизированных строк.
    non_standardized_rows_n = sum(priznak_standartizacii_da_net == "нет", na.rm = TRUE),
    
    # Завершаем группировку на уровне процедуры.
    .groups = "drop"
  ) %>%
  mutate(
    # Рассчитываем долю стандартизированной суммы спецификации внутри процедуры.
    standardized_specification_share = standardized_specification_sum / specification_sum,
    
    # Классифицируем процедуру по степени стандартизации.
    procedure_standardization_type = case_when(
      standardized_rows_n == rows_n ~ "fully_standardized",
      standardized_rows_n == 0 ~ "non_standardized",
      TRUE ~ "partially_standardized"
    )
  )

unique(procedure_level$procedure_standardization_type) %>% length()
procedure_level  %>% group_by(procedure_standardization_type) %>% 
  summarise(
    n_procedures = n()
  )
