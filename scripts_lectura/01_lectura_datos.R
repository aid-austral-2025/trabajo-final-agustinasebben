
##### LECTURA DE ARCHIVOS PARA EL ARMADO DE LA BASE #####

#________________________________________________________

##### LIBRERÍAS #####

library(readxl)
library(dplyr)
library(purrr)
library(janitor)
library(here)
library(tidyr)
library(stringr)
library(readxl)

#________________________________________________________

##### IMPORTAR ARCHIVOS #####

##### Datos de consumos

#Lectura

obj_consumos <- c(
    here("datos_crudos", "22_23.xlsx"),
    here("datos_crudos", "23_24.xlsx"),
    here("datos_crudos", "24_25.xlsx")
)

base_consumos <- map_dfr(obj_consumos, ~ 
                   read_excel(.x) %>%
                   clean_names() %>% 
                   select(
                     "des_centro_de_costo", 
                     "des_clase_de_costo",
                     "ctd_total_reg",
                     "mes",
                     "ano",
                     "unidad_medida",
                   ) %>% 
                   filter(
                        mes > 0 &
                        des_clase_de_costo %in% 
                          c("GAS NATURAL DISTRIB", 
                            "COMB. GAS C",
                            "COMB. GAS A",
                            "COMB. LIQ FUELOIL DI",
                            "ELEC DISTRIB",
                            "AGUA DESMINERALIZADA",
                            "VAPOR DE ALTA DISTRI"))
)

#Pasar de formato largo a ancho

consumos <- base_consumos %>% 
  group_by(ano, mes, des_clase_de_costo) %>% 
  summarise(ctd_total_reg = sum(ctd_total_reg, na.rm = TRUE)) %>% 
  pivot_wider(
    names_from = "des_clase_de_costo",
    values_from = "ctd_total_reg",
    values_fill = list(ctd_total_reg = 0)) %>% 
  mutate(
    ano = as.character(ano),
    mes = as.character(mes))

##### Datos de paradas de línea

#Lectura

base_paradas <- read_excel(here("datos_crudos", "Paradas.xlsx"))

#Cambiar formato de la base a número

names(base_paradas) [-1] <- format(
  as.Date(as.numeric(names(base_paradas) [-1]), origin = "1899-12-30"),
  "%b-%y"
)

#Pasar de formato ancho a largo

paradas <- base_paradas %>% 
  pivot_longer(
    cols = -1,
    names_to = "periodo",
    values_to = "Hs_Paradas"
  ) %>%
  mutate(
    mes = str_sub(str_split_fixed(periodo, "-", 2)[, 1], 1, 3),
    ano = paste0("20", str_split_fixed(periodo, "-", 2)[, 2])
  ) %>% 
  select(-c(periodo, Hs)) %>% 
  mutate(
    mes = case_when(
      mes == "ene" ~ 1,
      mes == "feb" ~ 2, 
      mes == "mar" ~ 3, 
      mes == "abr" ~ 4, 
      mes == "may" ~ 5, 
      mes == "jun" ~ 6,
      mes == "jul" ~ 7, 
      mes == "ago" ~ 8, 
      mes == "sep" ~ 9, 
      mes == "oct" ~ 10, 
      mes == "nov" ~ 11, 
      mes == "dic" ~ 12)
    )

##### Datos de producción

#Lectura

base_prod <- read_excel(here("datos_crudos", "Produccion.xlsx"))

#Cambiar formato de la base a número

names(base_prod) [-1] <- format(
  as.Date(as.numeric(names(base_prod) [-1]), origin = "1899-12-30"),
  "%b-%y"
)

#Pasar de formato ancho a largo

produccion <- base_prod %>% 
  pivot_longer(
    cols = -1,               
    names_to = "periodo",
    values_to = "produccion"
  ) %>% 
  mutate(
    mes = str_sub(str_split_fixed(periodo, "-", 2)[, 1], 1, 3),
    ano = paste0("20", str_split_fixed(periodo, "-", 2)[, 2])
  ) %>% 
  select(-periodo) %>% 
  mutate(
    mes = case_when(
      mes == "ene" ~ 1,
      mes == "feb" ~ 2, 
      mes == "mar" ~ 3, 
      mes == "abr" ~ 4, 
      mes == "may" ~ 5, 
      mes == "jun" ~ 6,
      mes == "jul" ~ 7, 
      mes == "ago" ~ 8, 
      mes == "sep" ~ 9, 
      mes == "oct" ~ 10, 
      mes == "nov" ~ 11, 
      mes == "dic" ~ 12)
  )


#Pasar de formato largo a ancho columna "Producto"

produccion <- produccion %>% 
    pivot_wider(
    names_from = "Producto",
    values_from = "produccion",
    values_fill = list(produccion = 0)
  )
