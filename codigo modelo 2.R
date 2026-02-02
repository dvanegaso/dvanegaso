library(dplyr)
library(purrr)
library(tictoc)
library(mvtnorm)
library(readxl)

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

# ya me mamé, ese modelo anidado es mas inútil que Giordana
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
