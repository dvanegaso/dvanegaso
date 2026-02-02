#################################################
############# TRATAMIENTO DE DATOS ##############
#################################################

setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")
# 1) cargamos la base del ICFES
caso = read.csv("Examen_Saber_11_2022_2_Entrenamiento.txt", sep = ";")

# 2) limpiamos a los venecos de la base
caso = caso[caso$estu_nacionalidad == "COLOMBIA",]

# todos los colombianos tienen de residencia a Colombia

# la variable investifacion del icfes no está en esta base xdxd

# 4) borramos a los santafereños

caso = caso[caso$estu_depto_reside != "SAN ANDRES",]

# 5) borramos a los datos faltantes en ubicacion del colegio

caso = caso[!is.na(caso$cole_depto_ubicacion),]      # borrado depto na
caso = caso[!is.na(caso$cole_mcpio_ubicacion),]      # borrado mcpio na
caso = caso[!is.na(caso$punt_global),]               # borrado puntaje na
caso = caso[!is.na(caso$fami_educacionmadre),]       # borrado educacion madre na
caso = caso[!is.na(caso$fami_tienecomputador),]      # borrado computador na
caso = caso[!is.na(caso$fami_tieneinternet),]        # borrado internet na
caso = caso[!is.na(caso$fami_numlibros),]            # borrado num libros na
caso = caso[!is.na(caso$fami_estratovivienda),]      # borrado estrato na
caso = caso[!is.na(caso$estu_tieneetnia),]           # borrado etnia na


#################################################
################## VARIABLES ####################
#################################################

# VARIABLES NIVEL ESTUDIANTE

# nivel educativo de la madre

caso$fami_educacionmadre = ifelse(caso$fami_educacionmadre == "Postgrado" | 
         caso$fami_educacionmadre == "Educación profesional completa",
       1,
       0)

# acceso a computador en la casa

caso$fami_tienecomputador = ifelse(caso$fami_tienecomputador == "Si",
                                  1,
                                  0)

# acceso a internet en casa

caso$fami_tieneinternet = ifelse(caso$fami_tieneinternet == "Si",
                                   1,
                                   0)

# numero de libros en casa

# estrato socioeconómico

# pertenencia etnica

caso$estu_tieneetnia = ifelse(caso$estu_tieneetnia == "Si",
                                 1,
                                 0)

# VARIABLES NIVEL MUNICIPIO

# carga de bases
#mcpio = readxl::read_xlsx("LAFT.xlsx")

# carga de datos de CEDE educacion
CEDE_tab = read.delim("PANEL_DE_EDUCACION(2021).tab")
CEDE = cbind(CEDE_tab$codmpio,CEDE_tab$docentotal,CEDE_tab$alumntotal)

# carga de datos de CEDE características
CEDE_tab = readxl::read_xlsx("PANEL_CARACTERISTICAS_GENERALES(2021).xlsx")
CEDE = cbind(CEDE,CEDE_tab$nbi,CEDE_tab$pobl_tot,CEDE_tab$pobl_rur)

# carga de datos de CEDE características
CEDE_tab = readxl::read_xlsx("PANEL_CONFLICTO_Y_VIOLENCIA(2021).xlsx")
CEDE = cbind(CEDE,CEDE_tab$homicidios,CEDE_tab$desplazados_expulsion)

# en CEDE están guardados todos los datos de estas cosas

CEDE = as.data.frame(CEDE)
colnames(CEDE) = c("codmpio","docentotal","alumntotal","nbi","pobl_tot","pobl_rur","homicidios","desplazados_expulsion")

CEDE$codmpio = as.character(CEDE$codmpio)



# sumamos los valores respecto a cada municipio

library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

CEDE = CEDE %>%
  group_by(codmpio) %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = T)))


# docentes/estudiante

cods = readxl::read_xlsx("codigos_municipios_dane.xlsx")

# tasa de homicidios

LAFT = readxl::read_xlsx("C:/Users/thetr/OneDrive/Documentos/R/bayesiana/LAFT.xlsx")

# correcciones previas para que la base LAFT no empiece a molestar
LAFT$CODIGO[LAFT$CODIGO == "CAUCA-PIENDAMO - TUNIA"] = "CAUCA-PIENDAMO_TUNIA"     # cambio de nombre
LAFT$CODIGO[LAFT$CODIGO == "AMAZONAS-MIRITI - PARANA"] = "AMAZONAS-MIRITI_PARANA" # cambio de nombre



LAFT = LAFT %>%
  separate(CODIGO, into = c("Depto","Mcpio"),sep = "-",remove = F)

LAFT = LAFT[LAFT$Depto != "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA",]
LAFT$Depto[LAFT$Depto == "BOGOTA, D.C."] = "BOGOTA" # cambio de nombre
LAFT$Depto[LAFT$Mcpio == "BOGOTA, D.C."] = "BOGOTA" # cambio de nombre
caso$estu_depto_reside[caso$estu_depto_reside == "BOGOTA D.C."] = "BOGOTA"
LAFT$Depto[LAFT$Depto == "NORTE DE SANTANDER"] = "NORTE SANTANDER" # cambio de nombre
LAFT$Depto[LAFT$Depto == "NARIÑO"] = "NARINO" # cambio de nombre


# correcciones previas para que la base caso no empiece a molestar
caso$estu_mcpio_reside[caso$estu_mcpio_reside == "MIRITÍ - PARANÁ"] = "MIRITI_PARANA"    # cambio de nombre
caso$estu_mcpio_reside[caso$estu_mcpio_reside == "PIENDAMÓ - TUNÍA"] = "PIENDAMO_TUNIA"  # cambio de nombre

caso$estu_depto_reside = stri_trans_general(caso$estu_depto_reside, "Latin-ASCII")
caso$estu_mcpio_reside = stri_trans_general(caso$estu_mcpio_reside, "Latin-ASCII")


# LIMPIEZA LAFT

# deptos

LAFT$Depto = LAFT$Depto %>%
  str_to_lower() %>%                        # todo en minúsculas
  str_trim() %>%                            # elimina espacios al inicio y final
  str_replace_all("[[:punct:]]", "") %>%    # elimina puntuación: comas, puntos, guiones, etc.
  str_replace_all("\\s+", "") %>%           # elimina todos los espacios intermedios
  stri_trans_general("Latin-ASCII")         # elimina tildes (acentos)       

# mcpios

LAFT$Mcpio = LAFT$Mcpio %>%
  str_to_lower() %>%                        # todo en minúsculas
  str_trim() %>%                            # elimina espacios al inicio y final
  str_replace_all("[[:punct:]]", "") %>%    # elimina puntuación: comas, puntos, guiones, etc.
  str_replace_all("\\s+", "") %>%           # elimina todos los espacios intermedios
  stri_trans_general("Latin-ASCII")         # elimina tildes (acentos)  

# LIMPIEZA caso

# deptos
caso$estu_depto_reside = caso$estu_depto_reside %>%
  str_to_lower() %>%                        # todo en minúsculas
  str_trim() %>%                            # elimina espacios al inicio y final
  str_replace_all("[[:punct:]]", "") %>%    # elimina puntuación: comas, puntos, guiones, etc.
  str_replace_all("\\s+", "") %>%           # elimina todos los espacios intermedios
  stri_trans_general("Latin-ASCII")         # elimina tildes (acentos)  

# mcpios

caso$estu_mcpio_reside = caso$estu_mcpio_reside %>%
  str_to_lower() %>%                        # todo en minúsculas
  str_trim() %>%                            # elimina espacios al inicio y final
  str_replace_all("[[:punct:]]", "") %>%    # elimina puntuación: comas, puntos, guiones, etc.
  str_replace_all("\\s+", "") %>%           # elimina todos los espacios intermedios
  stri_trans_general("Latin-ASCII")         # elimina tildes (acentos)  
  
LAFT$Depto[LAFT$Depto == "valledelcauca"] = "valle"
LAFT$Mcpio[LAFT$Mcpio == "santiagodecali"] = "cali"
LAFT$Mcpio[LAFT$Mcpio == "sanjosedecucuta"] = "cucuta"
LAFT$Mcpio[LAFT$Mcpio == "sotarapaispamba"] = "sotara"
LAFT$Mcpio[LAFT$Mcpio == "sanjosedetoluviejo"] = "toluviejo"
LAFT$Mcpio[LAFT$Mcpio == "santacruzdemompox"] = "mompos"
LAFT$Mcpio[LAFT$Mcpio == "nuevobelendebajira"] = "belendebajira"
LAFT$Mcpio[LAFT$Mcpio == "cuaspudcarlosama"] = "cuaspud"
LAFT$Mcpio[LAFT$clave == "guainia_mapiripana_guainia"] = "mapiripana"
LAFT$Mcpio[LAFT$clave == "meta_mapiripan_meta"] = "mapiripan"



##############################################
################# pegado de bases ###########
#########################


caso$clave = paste(caso$estu_depto_reside, caso$estu_mcpio_reside, sep = "_")          # acá uso donde resida el alumno
LAFT$clave = paste(LAFT$Depto, LAFT$Mcpio, sep = "_")

combi = full_join(caso,LAFT,by = "clave")  

#summary(combi$RISK_VICTIM_2022)
# combinamos ambas bases de datos

combi = combi[!is.na(combi$RISK_VICTIM_2022),]

names(combi)[names(combi) == "cole_cod_mcpio_ubicacion"] = "codmpio"    # cambio el nombre para combinar

combi$codmpio = as.character(combi$codmpio)      # hacemos esta cosa character

combi = full_join(combi, CEDE, by = "codmpio") # combinamos ambas segun el codigo del municipio

MOE = as.data.frame(readxl::read_xlsx("MOE - Riesgo por factores de Violencia.xlsx"))

MOE$Depto = toupper(stri_trans_general(MOE$Depto, "Latin-ASCII"))

names(MOE)[names(MOE) == "% municipios con riesgo"] = "porcentaje_en_riesgo"

MOE$Depto = MOE$Depto %>%
  str_to_lower() %>%                        # todo en minúsculas
  str_trim() %>%                            # elimina espacios al inicio y final
  str_replace_all("[[:punct:]]", "") %>%    # elimina puntuación: comas, puntos, guiones, etc.
  str_replace_all("\\s+", "") %>%           # elimina todos los espacios intermedios
  stri_trans_general("Latin-ASCII")         # elimina tildes (acentos) 

MOE$Depto[MOE$Depto == "valledelcauca"] = "valle"
MOE$Depto[MOE$Depto == "nortedesantander"] = "nortesantander"
MOE$Depto[MOE$Depto == "bogotadc"] = "bogota"

combi <- combi %>% filter(!is.na(estu_depto_reside))

combi = combi %>%
  left_join(MOE,by = "Depto")
summary(combi$porcentaje_en_riesgo)

datos = combi[,c(2,30,31,35,44,80,51,57,59,60,64,67,108,152,153,154,155,156,157,158,159)]

datos$porcentaje_en_riesgo = datos$porcentaje_en_riesgo/100

View(datos)

write.table(datos, file = "datazos.txt", sep = ";", row.names = FALSE)


