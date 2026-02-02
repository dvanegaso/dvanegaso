rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(tictoc)
# library(ggplot2)

setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")

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

B = 1

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

plot(SIMUL_GLOBAL$ALPHA, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$BETA, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$TAU, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$SIGMA, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$MU, type = "p", pch = 4,)
plot(SIMUL_GLOBAL$LL, type = "p", pch = 4,)

plot(zetajk_store[, "13_13030"], type = "l", ylab = expression(zeta[jk]),
     xlab = "Iteración")

# O calcula medias posteriores:
posterior_mean_zetas <- colMeans(zetajk_store)
posterior_mean_kappas <- colMeans(kappajk_store)


