library(dplyr)
library(tidyr)
library(purrr)
# library(ggplot2)

setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")

datos = read.csv("datazos.txt", sep = ";")   # se toman solamente los puntajes, deptos y mcpios

datos = as.data.frame(datos)

datos$estu_cod_reside_depto = as.character(datos$estu_cod_reside_depto)
datos$estu_cod_reside_mcpio = as.character(datos$estu_cod_reside_mcpio)

(d = length(table(datos$estu_cod_reside_depto)))       # número de departamentos
(m = length(table(datos$estu_cod_reside_mcpio)))       # número de municipios
(n = dim(datos)[1])                                    # número de estudiantes

nu_k = 1;nu_sig = 3;mu_mu0 = 250;sig_mu2 = 1;sig_tau2 = 150;nu_tau = 3;a_sig = 33;b_sig = 0.1

a_alph_kappa = 1
b_alph_kappa = 1
a_beta_kappa = 1
b_beta_kappa = 1

alphakappa = 1
betakappa = 1

# muestreador de gobbs


mun = datos %>%
  group_by(estu_cod_reside_depto, estu_cod_reside_mcpio) %>%
  summarise(
    zetajk = mean(punt_global),
    kappajk = var(punt_global),
    njk = n(),
    s1 = sum(punt_global),
    s2 = sum(punt_global^2),
    .groups = "drop"
  )

depto = datos %>%
  group_by(estu_cod_reside_depto) %>%
  summarise(
    thetak = mean(punt_global),     # theta de cada departamento                        (SERÁ SIMULADO)
    sigmak = var(punt_global),      # sigma de cada departamento                        (SERÄ SIMULADO)
    nj = n(),                       # número de estudiantes por cada departamento
    y_bark = thetak,                # media muestral de cada departamento
    s2k = sigmak,                   # varianza muestral de cada departamento
    kappak = 100,
    alphakappa = 1,
    betakappa = 1,
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
          kappajk                   # kappa de cada municipio                           (SERÁ SIMULADO)
      )
    )
  )
depto = full_join(depto, dep, by = "estu_cod_reside_depto")

depto = depto %>% 
  mutate(nm = map_int(mun, nrow)    # número de municipios por cada departamento
         )
  
mu = mean(datos$punt_global)        # mu de todo el país                                 (SERÁ SIMULADO)
tau = var(datos$punt_global)        # tau de todo el país                                (SERÄ SIMULADO)


sigma = NULL

for (k in 1:d) {
  for (j in 1:depto$nm[k]) {
    sigma = rbind(sigma, depto$mun[[k]]$zetajk[j])
  }
}

sigma = var(sigma)                 # sigma de todo el país                              (SERÁ SIMULADO)

# Metrópolis Pt1: The Miracle & The Sleeper :V

cuenta_alpha = 0
tuning1alpha = 1
phit = 0
alphat = alphakappa

alphak_function = function(alphakappa, sumkappa, sumlogkappa){
  d * ((alphakappa / 2) * log (betakappa / 2) - gamma(alphakappa / 2)) + 
    ((alphakappa / 2) - 1) * sumlogkappa + 
    (a_alph_kappa - 1) * log(alphakappa) - 
    (betakappa / 2) * sumkappa + 
    b_alph_kappa * alphakappa
}

# Metrópolis Pt2: Scenes From A Memory :vvv

cuenta_beta = 0
tuning1beta = 0.45
psit = 0
betat = betakappa

betak_function = function(betakappa, sumkappa){
  (d * alphakappa / 2) * log (betakappa / 2) + (a_beta_kappa - 1) * log(betakappa) - betakappa * (b_beta_kappa + (sumkappa / 2))
}

B = 200

SIMUL = matrix(NA, ncol = m + m + d + d + d + 4 + 2, nrow = B)
ALPHA = NULL
BETA = NULL
TAU = NULL
SIGMA = NULL
MU = NULL
KAPPA = NULL

tic()
for (l in 1:B) {
  for (k in 1:d) {
    for (j in 1:depto$nm[k]) {
      # ZETA JK
      zetajk_mu = ( depto$thetak[k] / depto$sigmak[k] ) + ( depto$mun[[k]]$y_barjk[j] * depto$mun[[k]]$njk[j] / depto$mun[[k]]$s2jk[j] )
      zetajk_sig = 1 / ( ( 1 / depto$sigmak[k] ) + ( depto$mun[[k]]$njk[j] / depto$mun[[k]]$s2jk[j]) )
      depto$mun[[k]]$zetajk[j] = rnorm(n = 1, mean = zetajk_mu * zetajk_sig , sd = sqrt(zetajk_sig))
      
      # KAPPA JK
      
      kappajk_alpha = (depto$mun[[k]]$njk[j] + nu_k)/2
      kappajk_beta = ((nu_k * depto$kappak[k] + ((depto$mun[[k]]$s2[j]) - (2 * depto$mun[[k]]$s1[j] * depto$mun[[k]]$zetajk[j]) + (depto$mun[[k]]$njk[j] * depto$mun[[k]]$zetajk[j])^2)))/2
      depto$mun[[k]]$kappajk[j] = 1/rgamma(n = 1, shape = kappajk_alpha , rate = kappajk_beta)
    }
    
    # THETA K
    thetak_mu = (mu/tau) + (( mean(depto$mun[[k]]$zetajk) * depto$nm[k] )/ depto$sigmak[k] )
    thetak_sig = 1 / (( 1 / tau ) + ( depto$nm[k] / depto$sigmak[k] ))
    depto$thetak[k] = rnorm(n = 1, mean = thetak_mu * thetak_sig, sd = sqrt(thetak_sig))
    
    # SIGMA K
    sigmak_alpha = (nu_sig + depto$nm[k])/2
    sigmak_beta = (nu_sig*sigma + sum((depto$mun[[k]]$zetajk - depto$thetak[k])^2)) / 2
    depto$sigmak[k] = 1/rgamma(n = 1, shape = sigmak_alpha, rate = sigmak_beta)
    
    # KAPPA K
    
    kappak_alpha = ((depto$nm[k] * nu_k) + alphakappa) / 2
    kappak_beta = (betakappa + (nu_k*sum(1/depto$mun[[k]]$kappajk))) / 2
    depto$kappak[k] = rgamma(n = 1, shape = kappak_alpha, rate = kappak_beta)
    
  }
  KAPPA = rbind(KAPPA, mean(depto$kappak))
  
  # MU
  
  mu_mu = (mu_mu0 / sig_mu2) + (mean((depto$thetak)*d)/tau)
  sigma_mu = 1/((1/sig_mu2) + (d/tau))
  mu = rnorm(n = 1, mean = mu_mu * sigma_mu, sigma_mu)
  MU = rbind(MU, mu)
  
  # SIGMA
  
  sigma_alpha = ( d * nu_sig + a_sig) / 2
  sigma_beta = ( b_sig + ( nu_sig * sum( 1 / depto$sigmak ))) / 2
  sigma = rgamma(n = 1, shape = sigma_alpha, rate = sigma_beta)
  SIGMA = rbind(SIGMA, sigma)
  
  # TAU
  
  tau_alpha = (d + nu_tau) / 2
  tau_beta = ((nu_tau * sig_tau2) + sum((depto$thetak - mu)^2)) / 2
  tau = 1/rgamma(n = 1, shape = tau_alpha, rate = tau_beta)
  TAU = rbind(TAU, tau)
  
  # METROPOLIS ALPHA
  
  phip = rnorm(n = 1, mean = phit,sd = tuning1alpha)
  alphap = exp(phip)
  ralpha = exp(alphak_function(alphakappa = alphap, sumkappa = sum(depto$kappak), sumlogkappa = sum(log(depto$kappak))) - alphak_function(alphakappa = alphat, sumkappa = sum(depto$kappak), sumlogkappa = sum(log(depto$kappak))) + phip - phit)
  
  if (runif(1) < ralpha) {
    alphakappa = alphap
    phit = phip
    ALPHA = rbind(ALPHA, alphakappa)
    cuenta_alpha = cuenta_alpha + 1 
  }
  
  # METROPOLIS BETA
  
  psip = rnorm(n = 1, mean = psit, sd = tuning1beta)
  betap = exp(psip)
  rbeta = exp(betak_function(betakappa = betap, sumkappa = sum(depto$kappak)) - betak_function(betakappa = betakappa, sumkappa = sum(depto$kappak)) + psip - psit)
  
  if (runif(1) < rbeta) {
    betakappa = betap
    psit = psip
    BETA = rbind(BETA, betakappa)
    cuenta_beta = cuenta_beta + 1
  }
  
  # LOG LIKELYHOOD
  
  
  
}
toc()
length(ALPHA)/B
length(BETA)/B
summary(MU)
summary(SIGMA)
summary(TAU)
summary(KAPPA)
