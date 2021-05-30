# Limpiar entorno de trabajo
rm(list = ls())

# Librerias --------------------------------------------------------------------
library(openxlsx)
library(dplyr)

# Diccionario ------------------------------------------------------------------

# lectura diccionario con variables de interes
diccionario <- read.xlsx("Diccionario.xlsx", sheet = "Variables")

# Seleccionar unicamente variables de interes
diccionarioInteres <- diccionario %>% 
  pull(codigo)

# Lectura BDD ------------------------------------------------------------------

# lectura bases de datos
personas <- read.delim("ENLEC_ADMIN_PERSONAS.txt")
vivienda <- read.delim("ENLEC_ADMIN_VIVIENDA.txt")
lectura <- read.delim("ENLEC_FORM_LECTURA.txt")

# generar ID unico por encuesta
personasID <- personas %>% 
  mutate(ID = paste0(FK_ID_FORMULARIO, "_", ID_VIVIENDA, 
                      "_", NUMERO_HOGAR, "_", NUMERO_PERSONA)) %>% 
  mutate(IDV = paste0(FK_ID_FORMULARIO, "_", ID_VIVIENDA)) 

viviendaID <- vivienda %>% 
  mutate(IDV = paste0(FK_ID_FORMULARIO, "_", ID_VIVIENDA)) 

lecturaID <- lectura %>% 
  mutate(ID = paste0(FK_ID_FORMULARIO, "_", ID_VIVIENDA, 
                     "_", NUMERO_HOGAR, "_", NUMERO_PERSONA))

# BDD definitiva ---------------------------------------------------------------
baseDatosDefinitiva <- lecturaID %>% 
  left_join(personasID, by = "ID") %>% 
  left_join(viviendaID, by = "IDV") %>% 
  select(ID, any_of(diccionarioInteres)) 

# Exportando la base de datos a Excel
write.xlsx(baseDatosDefinitiva, "baseConsolidadaENLEC.xlsx")

