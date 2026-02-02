rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(tictoc)
library(mvtnorm)
# library(ggplot2)

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


B =1

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
  
  # el guardado me lo paso por los huevos hasta que esta cosa funcione
  
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
