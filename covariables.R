library(dplyr)
library(purrr)
library(tictoc)
library(mvtnorm)
library(readxl)



datos = read.csv("datazos.txt", sep = ";")

datos = as.data.frame(datos)

tasa_dezplazamiento = datos$desplazados_expulsion/datos$pobl_tot * 100000
tasa_homicidios = datos$homicidios/datos$pobl_tot * 100000
docentes_por_estudiante = datos$docentotal/datos$alumntotal * 100

# codificacion de fami_numlibros para hacerla multiplicable

datos$fami_numlibros[datos$fami_numlibros == "0 A 10 LIBROS"] = 0
datos$fami_numlibros[datos$fami_numlibros == "11 A 25 LIBROS"] = 1
datos$fami_numlibros[datos$fami_numlibros == "26 A 100 LIBROS"] = 2
datos$fami_numlibros[datos$fami_numlibros == "MÁS DE 100 LIBROS"] = 3

# codifición de fami_estratovivienda para hacerla multiplicable
datos$fami_estratovivienda[datos$fami_estratovivienda == "Sin Estrato"] = 0
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 1"] = 1
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 2"] = 2
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 3"] = 3
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 4"] = 4
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 5"] = 5
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 6"] = 6

# sin esto, el modelo jode de una forma tenaz

DANE = readxl::read_xlsx("DANE - PIB.xlsx", sheet = "Cuadro 3", range = "A10:T43")

DANE = as.data.frame(DANE[,c(1,2,20)])

colnames(DANE) = c("estu_cod_reside_depto", "departamento", "PIB")

DANE$estu_cod_reside_depto = as.integer(DANE$estu_cod_reside_depto)

datos = full_join(datos, DANE, by = "estu_cod_reside_depto")
datos = datos[!datos$estu_cod_reside_depto == 88,]
datos$fami_estratovivienda = as.numeric(datos$fami_estratovivienda)
datos$fami_numlibros = as.numeric(datos$fami_numlibros)

########################################################################
##################### ESTRUCTURA DE DATOS ##############################
########################################################################

# dataframe del studiante (X)
est <- datos %>% transmute(
  estu_cod_reside_depto  = estu_cod_reside_depto,
  estu_cod_reside_mcpio  = estu_cod_reside_mcpio,
  y         = punt_global,
  etnia     = as.numeric(estu_tieneetnia),               #covariable
  madre     = as.numeric(fami_educacionmadre),           #covariable
  estrato   = as.numeric(fami_estratovivienda),          #covariable
  libros    = as.numeric(fami_numlibros),                #covariable
  compu     = as.numeric(fami_tienecomputador),          #covariable
  internet  = as.numeric(fami_tieneinternet)             #covariable
)
# Eliminar casos con NA en X o y
est <- est %>% filter(complete.cases(est))

# Guardado de matriz del estudiante
X_mat   <- as.matrix(est %>% select(etnia, madre, estrato, libros, compu, internet))
punt_global   <- est$y
estu_cod_reside_mcpio <- est$estu_cod_reside_mcpio
estu_cod_reside_depto <- est$estu_cod_reside_depto

# dataframe del municipio (Z)
mun <- datos %>%
  group_by(estu_cod_reside_depto, estu_cod_reside_mcpio) %>%
  summarise(
    nbi          = first(nbi),                                    #covariable
    doc_por_est  = first(docentotal/alumntotal),                  #covariable
    tasa_dezplaz = first((desplazados_expulsion/pobl_tot)*100000),#covariable
    tasa_homic   = first((homicidios/pobl_tot)*100000),           #covariable
    risk_victim  = first(RISK_VICTIM_2022),                       #covariable
    media_mun    = mean(punt_global),
    var_mun      = var(punt_global),
    .groups = "drop"
  ) %>% filter(complete.cases(.))

# Guardado de la matriz del municipio
Z_mat <- as.matrix(mun %>% select(nbi, doc_por_est, tasa_homic, tasa_dezplaz, risk_victim))

# dataframe del epartamento (W)
depto <- datos %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(
    pib          = first(PIB) / 1000000,                          #covariable
    pobl_rur     = first((pobl_rur / pobl_tot) * 100),            #covariable
    tasa_dezplaz = first((desplazados_expulsion/pobl_tot)*100000),#covariable
    tasa_homic   = first((homicidios/pobl_tot)*100000),           #covariable
    riesgo       = first(porcentaje_en_riesgo),                   #covariable
    .groups = "drop"
  ) %>% filter(complete.cases(.))