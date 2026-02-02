rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(tictoc)
# library(ggplot2)

setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")

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

datos = read.csv("datazos.txt", sep = ";")   # se toman solamente los puntajes, deptos y mcpios

datos = as.data.frame(datos)

datos$estu_cod_reside_depto = as.character(datos$estu_cod_reside_depto)
datos$estu_cod_reside_mcpio = as.character(datos$estu_cod_reside_mcpio)

(d = length(table(datos$estu_cod_reside_depto)))       # número de departamentos
(m = length(table(datos$estu_cod_reside_mcpio)))       # número de municipios
(n = dim(datos)[1])                                    # número de estudiantes

# muestreador de gobbs

y = datos %>%
  group_by(estu_cod_reside_depto, estu_cod_reside_mcpio) %>%
  summarise(
    y = list(punt_global),
    .groups = "drop"
  )

mun = datos %>%
  group_by(estu_cod_reside_depto, estu_cod_reside_mcpio) %>%
  summarise(
    zetajk = mean(punt_global),
    kappajk = var(punt_global),
    njk = n(),
    s1 = sum(punt_global),
    s2 = sum(punt_global^2),
    LL = zetajk,
    .groups = "drop"
  )

depto = datos %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(
    thetak = mean(punt_global),     # theta de cada departamento                        (SERÁ SIMULADO)
    kappak1 = var(punt_global),      # sigma de cada departamento                        (SERÄ SIMULADO)
    #    nj = n(),                       # número de estudiantes por cada departamento
    y_bark = thetak,                # media muestral de cada departamento
    s2k = kappak1,                   # varianza muestral de cada departamento
    kappak = kappak1,
    .groups = "drop"
  )


dep = mun %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(
    mun = list(
      tibble(
        estu_cod_reside_mcpio,
        y_barjk = zetajk,         # media muestral de cada municipio
        s2jk = kappajk,           # varianza muestral de cada municipio
        njk,                      # número de estudiantes por cada municipio
        zetajk,                   # zeta de cada municipio                            (SERÁ SIMULADO)
        s1,                       # suma de puntajes de cada municipio
        s2,                       # suma de cuadrados de puntajes de cada municipio
        kappajk,                  # kappa de cada municipio                           (SERÁ SIMULADO)
        LL
      )
    )
  )
depto = full_join(depto, dep, by = "estu_cod_reside_depto")

depto = depto %>% 
  mutate(nm = map_int(mun, nrow),    # número de municipios por cada departamento
         sigmak = map_dbl(mun, ~var(.x$zetajk)),  # varianza de 
         s2k = sigmak
  )

depto$s2k[1] = 1
depto$sigmak[1] = 1

depto1 = depto # guardamos una copia de esta cosa para no cargar el codigo desde cero

#  _______  _______  _______  _______ 
#(       )(  ____ \(       )(  ____ \
#! () () || (    \/| () () || (    \/
#! || || || |      | || || || |      
#! |(_)| || |      | |(_)| || |      
#! |   | || |      | |   | || |      
#! )   ( || (____/\| )   ( || (____/\
#!/     \|(_______/|/     \|(_______/


# si va a cargar el algoritmo hágalo desde aca

depto = depto1    # si quiere reiniciar

# número de iteraciones ara el algoritmo

B = 10000

# hiperparámetros (la mayoría funcionan medio bien)

nu_k = 0.5 ;nu_sig = 3;
mu_mu0 = mean(datos$punt_global)
sig_mu2 = 1
sig_tau2 = 150
nu_tau = 3
a_sig = 1
b_sig = 1

# parámetros del país

mu = mean(datos$punt_global, na.rm = TRUE)        # mu de todo el país                                 (SERÁ SIMULADO)
tau = var(datos$punt_global, na.rm = TRUE)        # tau de todo el país                                (SERÄ SIMULADO)


sigma = NULL

for (k in 1:d) {
  for (j in 1:depto$nm[k]) {
    sigma = rbind(sigma, depto$mun[[k]]$zetajk[j])
  }
}

sigma = var(sigma)                 # sigma de todo el país                              (SERÁ SIMULADO)

# hiperparámetros del REMALPARIDO KAPPA DEPARTAMENTAL QUE NO QUIERE ESTIMAR BIEN

a_alph_kappa = 10   #mean(depto$kappak1)
b_alph_kappa = 1
a_beta_kappa = 1
b_beta_kappa = 1

alphakappa = a_alph_kappa/b_alph_kappa   # valor inicial de alpha_kappa_k
betakappa = a_beta_kappa/b_beta_kappa    # valor inicial de beta_kappa_k
phit = log(alphakappa)

# Metrópolis del BETA

cuenta_alpha = 0
tuning1alpha = 0.15  # tuning parámeter (queremos una tasa de aceptacion entre 30% y 50%)

SIMUL_GLOBAL = matrix(NA, ncol = 6, nrow = B) # en algún momento tocará guardar toda la informacion, el algoritmo por ahora no guarda NADA, solo la última iteración.

colnames(SIMUL_GLOBAL) = c("ALPHA", "BETA", "TAU", "SIGMA", "MU", "LL")


mun_codes <- mun %>%
  arrange(estu_cod_reside_depto, estu_cod_reside_mcpio) %>%
  transmute(code = paste0(estu_cod_reside_depto, "_", estu_cod_reside_mcpio)) %>%
  pull(code)

M <- length(mun_codes)    # número total de municipios

# Pre‑asignamos matrices para almacenar:
zetajk_store  <- matrix(NA, nrow = B, ncol = M,
                        dimnames = list(NULL, mun_codes))
kappajk_store <- matrix(NA, nrow = B, ncol = M,
                        dimnames = list(NULL, mun_codes))

tic()
set.seed(123)
for (l in 1:B) {
  for (k in 1:d) {
    for (j in 1:depto$nm[k]) {
      
      # ZETA JK
      zetajk_mu = ( depto$thetak[k] / depto$sigmak[k] ) + ( depto$mun[[k]]$y_barjk[j] * depto$mun[[k]]$njk[j] / depto$mun[[k]]$s2jk[j] )
      zetajk_sig = 1 / ( ( 1 / depto$sigmak[k] ) + ( depto$mun[[k]]$njk[j] / depto$mun[[k]]$s2jk[j]) )
      depto$mun[[k]]$zetajk[j] = rnorm(n = 1, mean = zetajk_mu * zetajk_sig , sd = sqrt(zetajk_sig))
      
      # KAPPA JK
      
      kappajk_alpha = (depto$mun[[k]]$njk[j] + nu_k)/2
      kappajk_beta = ((nu_k * depto$kappak[k]) + ((depto$mun[[k]]$s2[j]) - (2 * depto$mun[[k]]$s1[j] * depto$mun[[k]]$zetajk[j]) + depto$mun[[k]]$njk[j] * (depto$mun[[k]]$zetajk[j])^2))/2
      depto$mun[[k]]$kappajk[j] = 1 / rgamma(n = 1, shape = kappajk_alpha , rate = kappajk_beta)
      
      # LOG-LIKELYHOOD
      
      depto$mun[[k]]$LL[j] = - (depto$mun[[k]]$njk[j] * log(2 * pi * depto$mun[[k]]$kappajk[j]) / 2) - (depto$mun[[k]]$s2[j] - 2 * depto$mun[[k]]$zetajk[j] * depto$mun[[k]]$s1[j] + depto$mun[[k]]$njk[j] * (depto$mun[[k]]$zetajk[j])^2) / (2 * depto$mun[[k]]$kappajk[j])
      
    }
    
    
    
    # THETA K
    thetak_mu = (mu/tau) + (( mean(depto$mun[[k]]$zetajk) * depto$nm[k] )/ depto$sigmak[k] )
    thetak_sig = 1 / (( 1 / tau ) + ( depto$nm[k] / depto$sigmak[k] ))
    depto$thetak[k] = rnorm(n = 1, mean = thetak_mu * thetak_sig, sd = sqrt(thetak_sig))
    
    # SIGMA K
    sigmak_alpha = (nu_sig + depto$nm[k])/2
    sigmak_beta = (nu_sig * sigma + sum((depto$mun[[k]]$zetajk - depto$thetak[k])^2)) / 2
    depto$sigmak[k] = 1/rgamma(n = 1, shape = sigmak_alpha, rate = sigmak_beta)
    
    # KAPPA K
    
    kappak_alpha = ((depto$nm[k] * nu_k) + alphakappa) / 2
    kappak_beta = (betakappa + (nu_k*sum(1/(depto$mun[[k]]$kappajk)))) / 2
    depto$kappak[k] = rgamma(n = 1, shape = kappak_alpha, rate = kappak_beta)
    
    
  }
  
  # MU
  
  mu_mu = (mu_mu0 / sig_mu2) + (mean((depto$thetak)*d)/tau)
  sigma_mu = 1/((1/sig_mu2) + (d/tau))
  mu = rnorm(n = 1, mean = mu_mu * sigma_mu, sqrt(sigma_mu))
  
  # SIGMA
  
  sigma_alpha = ( d * nu_sig + a_sig) / 2
  sigma_beta = ( b_sig + ( nu_sig * sum( 1 / depto$sigmak ))) / 2
  sigma = rgamma(n = 1, shape = sigma_alpha, rate = sigma_beta)
  
  # TAU
  
  tau_alpha = (d + nu_tau) / 2
  tau_beta = ((nu_tau * sig_tau2) + sum((depto$thetak - mu)^2)) / 2
  tau = 1/rgamma(n = 1, shape = tau_alpha, rate = tau_beta)
  
  # METROPOLIS ALPHA
  phip = rnorm(n = 1, mean = phit,sd = tuning1alpha)
  alphap = exp(phip)
  ralpha1 = (d * ((alphap / 2) * log (betakappa / 2) - lgamma( alphap / 2))) + (((alphap / 2) - 1) * sum(log(depto$kappak))) + ((a_alph_kappa - 1) * log(alphap)) - (b_alph_kappa * alphap)
  ralpha2 = (d * ((alphakappa / 2) * log (betakappa / 2) - lgamma( alphakappa / 2))) + (((alphakappa / 2) - 1) * sum(log(depto$kappak))) + ((a_alph_kappa - 1) * log(alphakappa)) - (b_alph_kappa * alphakappa)
  
  if (runif(1) < exp(ralpha1 - ralpha2 + phip - phit)) {
    alphakappa = alphap
    phit = phip
    cuenta_alpha = cuenta_alpha + 1
  }
  
  # BETA
  
  betakappa_alpha = ((alphakappa * d) / 2) + a_beta_kappa
  betakappa_beta = (sum(depto$kappak) / 2) + b_beta_kappa
  betakappa = rgamma(n = 1, shape = betakappa_alpha, rate = betakappa_beta)
  
  # LOG LIKELYHOOD
  
  SIMUL_GLOBAL[l,1] = alphakappa
  SIMUL_GLOBAL[l,2] = betakappa
  SIMUL_GLOBAL[l,3] = tau
  SIMUL_GLOBAL[l,4] = sigma
  SIMUL_GLOBAL[l,5] = mu
  SIMUL_GLOBAL[l,6] = sum(unlist(lapply(depto$mun, function(x) x$LL)), na.rm = TRUE)
  
  print(paste(cuenta_alpha/l))
  
  current_zetas <- unlist(
    lapply(depto$mun, function(df) df$zetajk)
  )
  # b) Y los kappas:
  current_kappas <- unlist(
    lapply(depto$mun, function(df) df$kappajk)
  )
  
  # c) Volcamos en la fila l de las matrices:
  zetajk_store[l, ]  <- current_zetas
  kappajk_store[l, ] <- current_kappas
  
}

toc()

SIMUL_GLOBAL = as.data.frame(SIMUL_GLOBAL)


# guardo archivo de seguridad
SIMUL_GLOBAL1 = SIMUL_GLOBAL

SIMUL_GLOBAL = as.data.frame(SIMUL_GLOBAL1[1:8000,1:6])
zetajk_store = as.data.frame(zetajk_store[1:8000,])
kappajk_store = as.data.frame(kappajk_store[1:8000,])

write.table(SIMUL_GLOBAL, file = "SIMUL_GLOBAL.txt", sep = ";", row.names = F)
write.table(zetajk_store, file = "zetajk_store.txt", sep = ";", row.names = F)
write.table(kappajk_store, file = "kappajk_store.txt", sep = ";", row.names = F)

plot(SIMUL_GLOBAL$ALPHA, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$BETA, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$TAU, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$SIGMA, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$MU, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$LL, type = "p", pch = 4,)
library(coda)



# O calcula medias posteriores:
posterior_mean_zetas <- colMeans(zetajk_store)
posterior_mean_kappas <- colMeans(kappajk_store)

# Calcular medias posteriores por municipio
posterior_means <- data.frame(
  code = as.character(mun_codes),
  mean_zeta = colMeans(zetajk_store),
  mean_kappa = colMeans(kappajk_store)
)

# Separar los códigos en departamento y municipio
posterior_means <- posterior_means %>%
  separate(code, into = c("depto_code", "mun_code"), sep = "_")

# Solución corregida para obtener los nombres de departamento
depto_names <- datos %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(estu_cod_reside_depto = first(as.character(estu_cod_reside_depto)),
         departamento = first(estu_depto_reside))  # Asegurar que sea character

# Ahora la unión debería funcionar
posterior_means <- posterior_means %>%
  left_join(depto_names, by = c("depto_code" = "estu_cod_reside_depto"))


# Calcular resumen por departamento
depto_summary <- posterior_means %>%
  group_by(depto_code) %>%
  summarise(
    departamento = first(departamento),
    q05 = quantile(mean_zeta, 0.05),
    avg_zeta = mean(mean_zeta),
    q95 = quantile(mean_zeta, 0.95),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_zeta))

# Mostrar el resumen
head(depto_summary, 10)
library(knitr)
kable(depto_summary, format = "latex")

write.table(depto_summary, file = "promedios departamentales.txt", sep = ";")

library(coda)

# 1. ESS para parámetros globales
ess_global <- apply(SIMUL_GLOBAL, 2, function(x) effectiveSize(as.mcmc(x)))

# 2. ESS para parámetros de municipio (zetajk y kappajk)
ess_zeta <- apply(zetajk_store, 2, function(x) effectiveSize(as.mcmc(x)))
ess_kappa <- apply(kappajk_store, 2, function(x) effectiveSize(as.mcmc(x)))

# Función para resumen extendido
resumen_5n <- function(x) {
  c(
    Mínimo = min(x),
    Q1 = quantile(x, 0.25),
    Mediana = median(x),
    Media = mean(x),
    Q3 = quantile(x, 0.75),
    Máximo = max(x)
  )
}

# Resumen por familia
tabla_resumen <- rbind(
  Globales = resumen_5n(ess_global),
  Zetajk = resumen_5n(ess_zeta),
  Kappajk = resumen_5n(ess_kappa)
)

# Mostrar tabla
print(tabla_resumen)
kable(tabla_resumen, format = "latex")


################################################################################
################################################################################
########################## MODELO 2 (o eso intentamos) #########################
################################################################################
################################################################################

# en resumen, no encontramos la forma de estimar con este modelo por la cantidad de tiempo que consumía 
# (una iteración por 10 minutos)

# cometimos el error de anidar operaciones, nos resultó en un modelo muy mal optimizado :/
# revisando otros métodos


setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")

datos = read.csv("datazos.txt", sep = ";")   # se toman solamente los puntajes, deptos y mcpios

datos = as.data.frame(datos)

tasa_dezplazamiento = datos$desplazados_expulsion/datos$pobl_tot * 100000
tasa_homicidios = datos$homicidios/datos$pobl_tot * 100000
docentes_por_estudiante = datos$docentotal/datos$alumntotal * 100

# recodificamos rápidamente fami_numlibros para hacerla multiplicable
datos$fami_numlibros[datos$fami_numlibros == "0 A 10 LIBROS"] = 0
datos$fami_numlibros[datos$fami_numlibros == "11 A 25 LIBROS"] = 1
datos$fami_numlibros[datos$fami_numlibros == "26 A 100 LIBROS"] = 2
datos$fami_numlibros[datos$fami_numlibros == "MÁS DE 100 LIBROS"] = 3

# recodificamos rápidamente fami_estratovivienda para hacerla multiplicable
datos$fami_estratovivienda[datos$fami_estratovivienda == "Sin Estrato"] = 0
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 1"] = 1
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 2"] = 2
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 3"] = 3
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 4"] = 4
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 5"] = 5
datos$fami_estratovivienda[datos$fami_estratovivienda == "Estrato 6"] = 6

DANE = readxl::read_xlsx("DANE - PIB.xlsx", sheet = "Cuadro 3", range = "A10:T43")

DANE = as.data.frame(DANE[,c(1,2,20)])

colnames(DANE) = c("estu_cod_reside_depto", "departamento", "PIB")

DANE$estu_cod_reside_depto = as.integer(DANE$estu_cod_reside_depto)

datos = full_join(datos, DANE, by = "estu_cod_reside_depto")
datos = datos[!datos$estu_cod_reside_depto == 88,]
datos$fami_estratovivienda = as.numeric(datos$fami_estratovivienda)
datos$fami_numlibros = as.numeric(datos$fami_numlibros)

est = datos%>%
  group_by(estu_cod_reside_depto, estu_cod_reside_mcpio,estu_consecutivo) %>%
  summarise(punt_global = as.numeric(punt_global),
            etnia = as.numeric(estu_tieneetnia),
            madre = as.numeric(fami_educacionmadre),
            estrato = as.numeric(fami_estratovivienda),
            libros = as.numeric(fami_numlibros),
            compu = as.numeric(fami_tienecomputador),
            internet = as.numeric(fami_tieneinternet),
            .groups = "drop"
  )

tic()
est = est %>%
  rowwise() %>%
  mutate(
    X = list(matrix(c_across(c(etnia, madre, estrato, libros, compu, internet)), nrow = 1)),
    XtX = list(crossprod(X))
  ) %>%
  ungroup()
toc()

mun = datos %>%
  group_by(estu_cod_reside_depto, estu_cod_reside_mcpio) %>%
  summarise(media_mun = mean(punt_global),
            var_mun = var(punt_global),
            nbi = as.numeric(first(nbi)),
            doc_por_est = as.numeric(first(docentes_por_estudiante)),
            tasa_homic = as.numeric(first((homicidios/pobl_tot)*100)),
            tasa_despl = as.numeric(first((desplazados_expulsion/pobl_tot)*100)),
            risk_victim = as.numeric(first(RISK_VICTIM_2022)),
            n_est = n(),
            .groups = "drop"
  )


mun = mun %>%
  rowwise() %>%
  mutate(
    Z = list(matrix(c_across(c(nbi,doc_por_est,tasa_despl,tasa_homic,risk_victim)), nrow = 1)),
    ZtZ = list(crossprod(Z))
  ) %>%
  ungroup()


depto = datos %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(media_dep = mean(punt_global),
            pib = as.numeric(first(PIB)/1000000),
            pobl_rur = as.numeric(first((pobl_rur/pobl_tot)*100)),
            tasa_despl = as.numeric(first((desplazados_expulsion/pobl_tot)*100000)),
            riesgo = as.numeric(first(porcentaje_en_riesgo)),
            tasa_homic = as.numeric(first((homicidios/pobl_tot)*100)),
            n_mun = as.numeric(n_distinct(estu_cod_reside_mcpio)),
            .groups = "drop"
  )

depto = depto %>%
  rowwise() %>%
  mutate(
    W = list(matrix(c_across(c(pib, pobl_rur, tasa_despl, tasa_homic, riesgo)), nrow = 1)),
    WtW = list(crossprod(W))
  ) %>%
  ungroup()

B = 1

e = dim(datos)[1]
d = length(table(datos$estu_cod_reside_depto))
m = length(table(datos$estu_cod_reside_mcpio))

################################################################################
################################################################################
########################### ESTRUCTURA DE DATOS ################################
################################################################################
################################################################################

# ajuste hiperparametros
mu_beta = 250
nu_beta = 1
gamma_beta = 1
mu_betaE = 0
nu_betaE = 1
gamma_betaE = 1
mu_betaM = 0
nu_betaM = 1
gamma_betaM = 1
mu_betaD = 0
nu_betaD = 1
gamma_betaD = 1
nu_kappak = 1
a_alpha_kappa = 1
b_alpha_kappa = 1
a_beta_kappa = 1
b_beta_kappa = 1

# valores iniciales de parámetros

E = matrix(data = 1, ncol = 1, nrow = 6)
D = matrix(data = 1, ncol = 1, nrow = 5)
M = matrix(data = 1, ncol = 1, nrow = 5)

set.seed(123)

# beta
sigma_beta = 1/rgamma(n = 1, shape = nu_beta / 2, rate = nu_beta*gamma_beta / 2)
beta = rnorm(n = 1, mean = mu_beta, sd = sqrt(sigma_beta))

sigma_betaE = 1/rgamma(n = 1, shape = nu_betaE / 2, rate = nu_betaE*gamma_betaE / 2)
betaE = rmvnorm(n = 1, mean = E*mu_betaE, sigma = diag(length(E))*sigma_betaE) * 0

sigma_betaM = 1/rgamma(n = 1, shape = nu_betaM / 2, rate = nu_betaM*gamma_betaM / 2)
betaM = rmvnorm(n = 1, mean = M * mu_betaM, sigma = diag(length(M)) * sigma_betaM) * 0

sigma_betaD = 1/rgamma(n = 1, shape = nu_betaD / 2, rate = nu_betaD*gamma_betaD / 2)
betaD = rmvnorm(n = 1, mean = D * mu_betaD, sigma = diag(length(D))*sigma_betaD) * 0
# kappa

alpha_kappa = rgamma(n = 1, shape = a_alpha_kappa, rate = b_alpha_kappa)
beta_kappa = rgamma(n = 1, shape = a_beta_kappa, rate = b_beta_kappa)
kappak = rgamma(n = 1, shape = alpha_kappa / 2, rate = beta_kappa / 2)
kappakj = 1/rgamma(n = 1, shape = nu_kappak / 2, rate = nu_kappak*kappak / 2)

# guardados 
kappa_init = 0


# mus
MU_BETA = 0
MU_BETAE = matrix(0, nrow = 6, ncol = 1)
MU_BETAM = matrix(0, nrow = 5, ncol = 1)
MU_BETAD = matrix(0, nrow = 5, ncol = 1)

# sigmas

TAU_BETA = 0
SIGMA_BETAE = matrix(0, nrow = 6, ncol = 6)
SIGMA_BETAM = matrix(0, nrow = 5, ncol = 5)
SIGMA_BETAD = matrix(0, nrow = 5, ncol = 5)

# guardados
BETA = NULL
BETAE = matrix(NA, nrow = B, ncol = 6)
BETAM = matrix(NA, nrow = B, ncol = 5) 
BETAD = matrix(NA, nrow = B, ncol = 5)

SIGMA = NULL
SIGMAE = NULL
SIGMAM = NULL
SIGMAD = NULL
ALPHA = NULL
BETA = NULL
KAPPAK = matrix(NA, nrow = 32, ncol = B)
KAPPAJK = matrix(NA, nrow = 1113, ncol = B)

zeta = NULL

################################################################################
################################################################################
################################ SIMULACIONES ##################################
################################################################################
################################################################################


B = 1

X_list <- est$X
Z_list <- mun$Z
W_list <- depto$W

tic()
for (l in 1:B) {
  
  
  
  for (k in 1:d) {
    print(k)
    W_prod = betaD%*%Wt
    SIGMA_BETAD_0 = Wt%*%W
    
    for (j in 1:depto$n_mun[k]) {
      tic()
      
      # SIMULACION DE KAPPAK
      
      
      # declaraciones previas
      Z = as.matrix(depto$mun[[k]][j,5:9])
      Zt = t(Z)
      Z_prod = betaM%*%Zt
      SIGMA_BETAM_0 = Zt%*%Z
      
      zeta = NULL
      
      MU_BETA_0 = 0
      MU_BETAE_0 = 0
      MU_BETAM_0 = 0
      MU_BETAD_0 = 0
      SIGMA_BETAE_0 = 0
      
      
      for (i in 1:depto$mun[[k]]$n_est[j]) {
        X = as.matrix(depto$mun[[k]]$est[[j]][i,2:7])
        Xt = t(X)
        X_prod = betaE%*%Xt
        zeta[i] = beta + X_prod + Z_prod + W_prod
        
        # medias incompletas
        MU_BETA_0 = depto$mun[[k]]$est[[j]][i,1] - X_prod - Z_prod - W_prod
        MU_BETAE_0 = (depto$mun[[k]]$est[[j]][i,1] - beta - Z_prod - W_prod)*Xt
        MU_BETAM_0 = (depto$mun[[k]]$est[[j]][i,1] - X_prod - beta - W_prod)*Zt
        MU_BETAD_0 = (depto$mun[[k]]$est[[j]][i,1] - X_prod - Z_prod - beta)*Wt
        
        # varianzas incompletas
        SIGMA_BETAE_0 = SIGMA_BETAE_0 + Xt%*%X
        
      }
      
      MU_BETA = MU_BETA + MU_BETA_0/depto$mun[[k]]$var_mun[j]
      MU_BETAE = MU_BETAE + MU_BETAE_0*(depto$mun[[k]]$n_est[j]/depto$mun[[k]]$var_mun[j])
      MU_BETAM = MU_BETAM + MU_BETAM_0*(depto$mun[[k]]$n_est[j]/depto$mun[[k]]$var_mun[j])
      MU_BETAD = MU_BETAD + MU_BETAD_0*(depto$mun[[k]]$n_est[j]/depto$mun[[k]]$var_mun[j])
      
      SIGMA_BETAE = SIGMA_BETAE + SIGMA_BETAE_0*(1/depto$mun[[k]]$var_mun[j])
      SIGMA_BETAM = SIGMA_BETAM + SIGMA_BETAM_0*(depto$mun[[k]]$n_est[j]/depto$mun[[k]]$var_mun[j])
      SIGMA_BETAD = SIGMA_BETAD + SIGMA_BETAD_0*(depto$mun[[k]]$n_est[j]/depto$mun[[k]]$var_mun[j])
      
      # incorporar zeta
      
      # SIMULACION DE KAPPASJK
      depto$mun[[k]]$kappajk[j] = 1/rgamma(n = 1, shape = depto$mun[[k]]$n_est[j]*nu_kappak / 2, rate = as.numeric(nu_kappak + sum((zeta - depto$mun[[k]]$est[[j]]$punt_global)^2)) / 2)
      
      
      TAU_BETA = TAU_BETA + 1/depto$mun[[k]]$var_mun[j]
      toc()
    }
    
  }
  
  # completamos los parámetros
  # completamos variaciones
  TAU_BETA = TAU_BETA + 1/sigma_beta
  SIGMA_BETAE = solve(SIGMA_BETAE + diag(6)/sigma_betaE)
  SIGMA_BETAM = solve(SIGMA_BETAM + diag(5)/sigma_betaM)
  SIGMA_BETAD = solve(SIGMA_BETAD + diag(5)/sigma_betaD)
  
  # completamos medias
  MU_BETA = (MU_BETA + mu_beta/sigma_beta)/TAU_BETA
  MU_BETAE = SIGMA_BETAE%*%(as.numeric(MU_BETAE) + (mu_betaE/sigma_betaE)*E)
  MU_BETAM = SIGMA_BETAM%*%(as.numeric(MU_BETAM) + (mu_betaM/sigma_betaM)*M)
  MU_BETAD = SIGMA_BETAD%*%(as.numeric(MU_BETAD) + (mu_betaD/sigma_betaD)*D)
  
  ################################################
  ################# SIMULACIONES #################
  ################################################
  
  # simulaciones de los betas
  beta = rnorm(n = 1, mean = as.numeric(MU_BETA),sd = 1/TAU_BETA)
  betaE = rmvnorm(n = 1, mean = MU_BETAE, sigma = SIGMA_BETAE)
  betaM = rmvnorm(n = 1, mean = MU_BETAM, sigma = SIGMA_BETAM)
  betaD = rmvnorm(n = 1, mean = MU_BETAD, sigma = SIGMA_BETAD)
  
  #simulaciones de los sigmas
  sigma_beta = 1/rgamma(n = 1, shape = (nu_beta + 1) / 2, rate = (nu_beta*gamma_beta + (beta - mu_beta)^2) / 2)
  sigma_betaE = 1/rgamma(n = 1, shape = (nu_betaE + e) / 2, rate = (nu_betaE*gamma_betaE + t( t(betaE) - mu_betaE*E )%*%( t(betaE) - mu_betaE*E )) / 2)
  sigma_betaM = 1/rgamma(n = 1, shape = (nu_betaM + m) / 2, rate = (nu_betaM*gamma_betaM + t( t(betaM) - mu_betaE*M )%*%( t(betaM) - mu_betaM*M )) / 2)
  sigma_betaD = 1/rgamma(n = 1, shape = (nu_betaD + d) / 2, rate = (nu_betaD*gamma_betaD + t( t(betaD) - mu_betaE*D )%*%( t(betaD) - mu_betaD*D)) / 2)
  
  # guardados
  
  BETA[l] = beta
  BETAE[l] = betaE
  BETAM[l] = betaM
  BETAD[l] = betaD
  
  SIGMA[l] = sigma_beta
  SIGMAD[l] = sigma_betaD
  SIGMAM[l] = sigma_betaM
  SIGMAE[l] = sigma_betaE
  
}
toc()

# demasiado tiempo (632sg)

# recargamos los datos

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
    doc_por_est  = first(docentotal/alumntotal),
    tasa_dezplaz_m = first((desplazados_expulsion/pobl_tot)*100000),
    tasa_homic_m   = first((homicidios/pobl_tot)*100000),          #covariable
    risk_victim  = first(RISK_VICTIM_2022),                       #covariable
    media_mun    = mean(punt_global),
    var_mun      = var(punt_global),
    n_j = n(),
    .groups = "drop"
  ) %>% filter(complete.cases(.))

# Guardado de la matriz del municipio
Z_mat <- as.matrix(mun %>% select(nbi, doc_por_est, tasa_homic_m, tasa_dezplaz_m, risk_victim))

# dataframe del epartamento (W)
depto <- datos %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(
    pib          = first(PIB) / 1000000,                          #covariable
    pobl_rur     = first((pobl_rur / pobl_tot) * 100),
    tasa_dezplaz_d = first((desplazados_expulsion/pobl_tot)*100000),
    tasa_homic_d   = first((homicidios/pobl_tot)*100000),          #covariable
    riesgo       = first(porcentaje_en_riesgo),                   #covariable
    var_depto    = var(punt_global),
    .groups = "drop"
  ) %>% filter(complete.cases(.))

datos1 = full_join(est, depto, by = "estu_cod_reside_depto")
datos1 = full_join(datos1, mun, by = "estu_cod_reside_mcpio")

modelo = lm(punt_global ~ etnia + madre + estrato + libros + compu + internet + 
              nbi + doc_por_est + tasa_dezplaz_m + tasa_homic_m + risk_victim + 
              pib + pobl_rur + tasa_dezplaz_d + tasa_homic_d + riesgo, data = datos1)
init = coefficients(modelo)

# ——— ESCALADO DE COVARIABLES ———

# A nivel estudiante: dejar binarios 0/1; escalar cont. ordinales
X_scaled <- est %>%
  mutate(
    etnia    = etnia,
    compu    = compu,
    internet = internet,
    madre    = scale(madre)[,1],
    estrato  = scale(estrato)[,1],
    libros   = scale(libros)[,1]
  )

# A nivel municipio: escalar todas las continuas
mun_scaled <- mun %>%
  mutate(across(c(nbi, doc_por_est, tasa_dezplaz_m, tasa_homic_m, risk_victim),
                ~ scale(.)[,1]))

# A nivel departamento: escalar todas las continuas
depto_scaled <- depto %>%
  mutate(across(c(pib, pobl_rur, tasa_dezplaz_d, tasa_homic_d, riesgo),
                ~ scale(.)[,1]))
y_s   <- scale(est$y)[,1]
# Construcción de matrices
X_mat <- as.matrix(X_scaled %>% select(etnia, madre, estrato, libros, compu, internet))
Z_mat <- as.matrix(mun_scaled %>% select(nbi, doc_por_est, tasa_homic_m, tasa_dezplaz_m, risk_victim))
W_mat <- as.matrix(depto_scaled %>% select(pib, pobl_rur, tasa_homic_d, tasa_dezplaz_d, riesgo))

# "Grandes" Z y W para cada estudiante
Z_big <- Z_mat[ match(estu_cod_reside_mcpio, mun_scaled$estu_cod_reside_mcpio), ]
W_big <- W_mat[ match(estu_cod_reside_depto, depto_scaled$estu_cod_reside_depto), ]

# Precomputados de conteo y varianzas iniciales
n_jk   <- setNames(mun$n_j,      mun$estu_cod_reside_mcpio)
kappa_j <- setNames(mun$var_mun, mun$estu_cod_reside_mcpio)
kappa_k <- setNames(depto$var_depto, depto$estu_cod_reside_depto)

# Guardado de la matriz del departamento
W_mat <- as.matrix(depto %>% select(pib, pobl_rur, tasa_dezplaz_d, riesgo, tasa_homic_d))

# Expandir por estudiante e inspeccionar NAs
Z_big <- Z_mat[match(estu_cod_reside_mcpio, mun$estu_cod_reside_mcpio), ]
W_big <- W_mat[match(estu_cod_reside_depto, depto$estu_cod_reside_depto), ]

########################################################################
##################### DECLARACIÓN DE PRIORIS ###########################
########################################################################

# Dimensiones y priors
B <- 15
p <- ncol(X_mat) # número de covariables estudiantiles
q <- ncol(Z_big) # número de covariables municipales
r <- ncol(W_big) # número de covariables departamentales
M <- nrow(mun)   # número de municipios
D <- nrow(depto) # número de departamentos
mu0 <- mean(datos$punt_global)
muE <- rep(1,p)
muM <- rep(1,q)
muD <- rep(1,r)
gamma1 = sd(residuals(lm(punt_global ~ etnia + madre + estrato + libros + compu + internet +
                           nbi + doc_por_est + tasa_dezplaz_m + tasa_homic_m + risk_victim + 
                           pib + pobl_rur + tasa_dezplaz_d + tasa_homic_d + riesgo, data = datos1)))
gammaE <- sd(residuals(lm(punt_global ~ etnia + madre + estrato + libros + compu + internet , data = datos1)))
gammaM <- sd(residuals((lm(punt_global ~ nbi + doc_por_est + tasa_dezplaz_m + tasa_homic_m + risk_victim, data = datos1))))
gammaD <- sd(residuals(lm(punt_global ~ pib + pobl_rur + tasa_dezplaz_d + tasa_homic_d + riesgo, data = datos1)))
nu_kappa <- 1
nu = 1
nuE = 1
nuD = 1
nuM = 1
a_alpha_kappa = 1
b_alpha_kappa = 1
a_beta_kappa = 1 
b_beta_kappa = 1
tuning1alpha = 0.15
d =32

########################################################################
##################### INICIALIZACIÓN Y AJUSTES #########################
########################################################################



beta = mu0
betaE  = muE
betaM = muM
betaD = muD
sigma1 = gamma1
sigmaE = gammaE
sigmaM = gammaM
sigmaD = gammaD
n_jk = setNames(mun$n_j, mun$estu_cod_reside_mcpio)
kappa_j = setNames(mun$var_mun, mun$estu_cod_reside_mcpio)
kappa_k = setNames(depto$var_depto, depto$estu_cod_reside_depto)
alphakappa = 1 
betakappa = 1
phit = log(alphakappa)

# guardados

BETA = NULL
BETAE = matrix(NA, B, p)
BETAM = matrix(NA, B, q)
BETAD = matrix(NA, B, r)
KAPPAJ = matrix(NA, B, M)
KAPPA = matrix(NA, B, D)
SIGMA = NULL
SIGMAE = NULL
SIGMAM = NULL
SIGMAD = NULL
ALPHA = NULL
BETA = NULL
LL = NULL

# desde hoy en adelante soy hater de los tibbles

# temporales de actualización

temp_beta <- function(mu, var_inv, y, mu_pred, w) {
  tau_beta <- var_inv + sum(w, na.rm = TRUE)
  m <- (mu * var_inv + sum(w * (y - mu_pred), na.rm = TRUE))/tau_beta
  rnorm(1, m, 1/sqrt(tau_beta))
}

temp_betavec <- function(XtX, XtY, pri_inv, mu) {
  S = solve(pri_inv + XtX)
  m = S %*% (XtY + pri_inv %*% mu)
  drop(rmvnorm(1, m, S, method = "chol"))
}

update_kappa_j <- function(n_j, SSR, nu, parent_kappa) {
  shape1 <- (nu + n_j) / 2
  rate1  <- (nu * parent_kappa + SSR) / 2
  1/rgamma(1, shape1, rate = rate1)
}

update_kappa_k <- function(n_k, sum_inv_kappa_j, nu, parent_alpha) {
  shape2 <- (nu + n_k) / 2
  rate2  <- (nu * parent_alpha + sum_inv_kappa_j) / 2
  rgamma(1, shape2, rate = rate2)
}

########################################################################
################### GIBBS (LA VENGANZA DE FISHER) ######################
########################################################################

cuenta_alpha = 0
set.seed(123)
tic()
for(iter in 1:B) {
  # Predicción y pesos (con límite para evitar valores extremos)
  w_inv = 1 / (kappa_j[as.character(estu_cod_reside_mcpio)] + .Machine$double.eps)
  lambda = n_jk[as.character(estu_cod_reside_mcpio)] / (kappa_j[as.character(estu_cod_reside_mcpio)] + .Machine$double.eps)
  
  # Actualizar parámetros con funciones robustas
  beta = temp_beta(mu = mu0,
                   var_inv =  1/sigma1,
                   y = punt_global,
                   mu_pred =  beta + X_mat %*% betaE + Z_big %*% betaM + W_big %*% betaD,
                   w = w_inv)
  
  # Coeficientes E
  betaE = temp_betavec(XtX = crossprod(sqrt(w_inv) * X_mat),
                       XtY = crossprod(sqrt(w_inv) * X_mat, (punt_global - beta - Z_big %*% betaM - W_big %*% betaD)),
                       pri_inv = diag(muE/sigmaE, p), 
                       mu = muE)
  
  # Coeficientes M
  betaM = temp_betavec(XtX = crossprod(sqrt(lambda) * Z_big),
                       XtY = crossprod(sqrt(lambda) * Z_big, (punt_global - beta - X_mat %*% betaE - W_big %*% betaD)),
                       pri_inv = diag(muM/sigmaM, q), 
                       mu = muM)
  
  # Coeficientes D
  betaD = temp_betavec(XtX = crossprod(sqrt(lambda) * W_big),
                       XtY = crossprod(sqrt(lambda) * W_big, (punt_global - beta - X_mat %*% betaE - Z_big %*% betaM)),
                       pri_inv = diag(muD/sigmaD, r),
                       mu =  muD)
  
  # Varianzas municipales (kappa_j)
  for(j in 1:M) {
    idx   <- which(estu_cod_reside_mcpio == mun$estu_cod_reside_mcpio[j])
    n_j   <- length(idx)
    if (n_j > 0) {
      # SSR en municipio j
      suma  <- sum((punt_global[idx] -
                      (beta
                       + X_mat[idx,] %*% betaE
                       + Z_big[idx,] %*% betaM
                       + W_big[idx,] %*% betaD))^2)
      # extraigo el departamento “padre” del primer individuo
      parent_idx   <- estu_cod_reside_depto[idx][1]
      parent_kappa <- kappa_k[parent_idx]
      
      # actualizo solo si parent_kappa no es NA
      if (!is.na(parent_kappa)) {
        kappa_j[j] <- update_kappa_j(n_j       = n_j,
                                     SSR       = suma,
                                     nu        = nu_kappa,
                                     parent_kappa = parent_kappa)
      }
      # else: dejo kappa_j[j] como estaba en la iteración previa
    }
    # else: municipio sin datos, no toco kappa_j[j]
  }
  
  # Varianzas departamentales (kappa_k)
  for(k in 1:D) {
    ids <- which(mun$estu_cod_reside_depto == depto$estu_cod_reside_depto[k])
    inv_kappa_sum <- sum(1 / kappa_j[ids])
    kappa_k[k] <- update_kappa_k(n_k = length(ids),
                                 sum_inv_kappa_j =  inv_kappa_sum,
                                 nu =  nu_kappa,
                                 parent_alpha =  alphakappa) 
  }
  
  # Hiperparámetros de varianza
  sigma1 <- 1/rgamma(n = 1, (nu+1)/2, (gamma1 + (beta - mu0)^2)/2)
  sigmaE <- 1/rgamma(n = 1, (p+nuE)/2, (gammaE*p + t(betaE - muE*rep(1,p))%*%(betaE - muE*rep(1,p)))/2)
  sigmaM <- 1/rgamma(n = 1, (q+nuM)/2, (gammaM*q + t(betaM - muM*rep(1,p))%*%(betaM - muM*rep(1,p)))/2)
  sigmaD <- 1/rgamma(n = 1, (r+nuD)/2, (gammaD*r + t(betaD - muD*rep(1,p))%*%(betaD - muD*rep(1,p)))/2)
  
  # METROPOLIS ALPHA
  
  ALPHA[iter] = alphakappa
  phip = rnorm(n = 1, mean = phit,sd = tuning1alpha)
  alphap = exp(phip)
  ralpha1 = (d * ((alphap / 2) * log (betakappa / 2) - lgamma( alphap / 2))) + (((alphap / 2) - 1) * sum(log(kappa_k))) + ((a_alpha_kappa - 1) * log(alphap)) - (b_alpha_kappa * alphap)
  ralpha2 = (d * ((alphakappa / 2) * log (betakappa / 2) - lgamma( alphakappa / 2))) + (((alphakappa / 2) - 1) * sum(log(kappa_k))) + ((a_alpha_kappa - 1) * log(alphakappa)) - (b_alpha_kappa * alphakappa)
  
  if (runif(1) < exp(ralpha1 - ralpha2 + phip - phit)) {
    alphakappa = alphap
    phit = phip
    cuenta_alpha = cuenta_alpha + 1
  }
  
  # BETA
  BETA[iter] = betakappa
  betakappa_alpha = ((alphakappa * d) / 2) + a_beta_kappa
  betakappa_beta = (sum(kappa_k) / 2) + b_beta_kappa
  betakappa = rgamma(n = 1, shape = betakappa_alpha, rate = betakappa_beta)
  
  # Guardar muestra
  BETA[iter] = beta
  BETAE[iter, ] = betaE
  BETAM[iter, ] = betaM
  BETAD[iter, ] = betaD
  KAPPAJ[iter, ] = kappa_j
  KAPPA[iter, ] = kappa_k
  SIGMA[iter] = sigma1
  SIGMAE[iter] = sigmaE
  SIGMAM[iter] = sigmaM
  SIGMAD[iter] = sigmaD
  LL[iter] = 0
  print(paste(cuenta_alpha/iter))
  # Mostrar progreso
  print(iter)
}
toc()
BETA
SIGMAE
#save(chain, file = "gibbs_chain_optimized.RData")


