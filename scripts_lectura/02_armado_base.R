
##### CONCATENAR LOS DATOS LEÍDOS #####


# Pasar a formato character las columnas "año" y "mes" para luego unir

consumos <- consumos %>% 
  mutate(
    mes = as.character(mes),
    ano = as.character(ano)
  )

paradas <- paradas %>% 
  mutate(
    mes = as.character(mes),
    ano = as.character(ano)
  )

produccion <- produccion %>% 
  mutate(
    mes = as.character(mes),
    ano = as.character(ano)
  )

# Unir bases

df <- produccion %>%
  full_join(paradas,  by = c("ano", "mes")) %>%
  full_join(consumos, by = c("ano", "mes"))

# Setear formato y nombres de columnas

df <- df %>% 
  mutate(across(.cols = -c(1, 2), as.numeric))


df <- df %>% 
  rename(
    consumo_gas_natural = `GAS NATURAL DISTRIB`,
    consumo_gas_linea_A = `COMB. GAS A`,
    consumo_gas_linea_C = `COMB. GAS C`,
    consumo_fuel_oil = `COMB. LIQ FUELOIL DI`,
    consumo_agua = `AGUA DESMINERALIZADA`,
    consumo_EE = `ELEC DISTRIB`,
    consumo_vapor = `VAPOR DE ALTA DISTRI`,
    prod_vapor_Gcal = `Vapor (Gcal)`,
    prod_EE_MWh = `Energía Eléctrica (MWh)`,
    prod_lineaA_tn = `Línea A (tn)`,
    prod_lineaC_Tn = `Línea C (tn)`,
  ) %>% 
  select(
    mes,
    ano,
    consumo_gas_natural,
    consumo_gas_linea_A,
    consumo_gas_linea_C,
    consumo_fuel_oil,
    consumo_agua,
    consumo_EE,
    consumo_vapor,
    prod_vapor_Gcal,
    prod_EE_MWh,
    prod_lineaA_tn,
    prod_lineaC_Tn,
    Hs_Paradas,
  )

# Agregar columna que indique estación del año

df <- df %>% 
  mutate(
    estacion = case_when(
      mes > 3 & mes < 9 ~ "Invierno",
      TRUE ~ "Verano"
    )
  )

# Agregar columna que concatene mes y año

df$periodo <- as.Date(paste(df$ano, df$mes, "01", sep = "-"))


# Agregar columna promedio por estación

df <- df %>% 
  mutate(
    grupo_estacion = cumsum(estacion != lag(estacion, default = first(estacion)))
  )

df <- df %>% 
  group_by(grupo_estacion) %>% 
  mutate(prom_GN = mean(consumo_gas_natural, na.rm = TRUE)) %>% 
  ungroup()

# Armar tabla con totales de consumo por gas (sin período)

df_TOT <- df %>% 
  summarise(
    consumo_gas_natural = sum(consumo_gas_natural, na.rm = TRUE),
    consumo_gas_linea_A = sum(consumo_gas_linea_A, na.rm = TRUE),
    consumo_gas_linea_C = sum(consumo_gas_linea_C, na.rm = TRUE),
    consumo_fuel_oil    = sum(consumo_fuel_oil, na.rm = TRUE),
    consumo_agua = sum(consumo_fuel_oil, na.rm = TRUE)
  ) %>% 
  rename(
    `Gas Natural` = consumo_gas_natural,
    `Gas Línea A` = consumo_gas_linea_A,
    `Gas Línea C` = consumo_gas_linea_C,
    `Fuel Oil`    = consumo_fuel_oil,
    `Agua`        = consumo_agua
  ) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variables",
    values_to = "Total"
  )

#Guardar dataframe para luego importar en quarto

saveRDS(df, file = "df.rds")
saveRDS(df_TOT, file = "df_TOT.rds")
