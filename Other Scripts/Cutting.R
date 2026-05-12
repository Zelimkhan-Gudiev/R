fat_percent <- 11.9
fat_kg <- 9.2

body_weight <- 77.6
fat_kg / body_weight * 100

target_fat_percent <- 0.05
target_fat_kg <- body_weight * target_fat_percent
target_fat_kg
fat_kg_difference <- fat_kg - target_fat_kg
kilocalories_dificit <-  fat_kg_difference * 8000
kilocalories_dificit / 1000
