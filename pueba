# Paso 1: Definir vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(NA, 25, 30, 28, NA, 35, 40, 38, 45, 50, 
             20, 22, 24, 26, 28, 30, 32, 34, 36, 38)
costo_kwh <- c(rep(0.12, 10), rep(0.15, 10))

# Paso 2: Limpiar datos - Reemplazar NA con la mediana por tipo de energía
for (tipo in unique(energia)) {
  mediana_consumo <- median(consumo[energia == tipo], na.rm = TRUE)
  consumo[energia == tipo & is.na(consumo)] <- mediana_consumo
}

# Paso 3: Crear dataframe
df_consumo <- data.frame(energia, consumo, costo_kwh)

# Paso 4: Calcular costo total
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Paso 5: Calcular métricas
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Paso 6: Simular aumento del 10% en ganancias
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 7: Crear lista resumen_energia
resumen_energia <- list(
  df_ordenado = df_consumo[order(-df_consumo$costo_total), ],
  total_consumo = total_consumo,
  total_costo = total_costo,
  media_consumo = media_consumo,
  top_3_costos = head(df_consumo[order(-df_consumo$costo_total), ], 3)
)

# Mostrar resumen
print(resumen_energia)
