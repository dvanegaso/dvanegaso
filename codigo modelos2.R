#==============================================================
# MCMC Jerárquico Bayesiano Optimizado en R
# Incluye cálculo de log-verosimilitud
#==============================================================

#--- 1. Librerías -----------------------------------------------------------
library(dplyr)
library(purrr)
library(tictoc)
library(mvtnorm)
library(readxl)

#--- 2. Carga y preprocesamiento de datos ----------------------------------
setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")
datos <- read.csv("datazos.txt", sep = ";")
datos <- read.csv("datazos.txt", sep = ";") %>%
  # derivadas y recodificaciones
  mutate(
    fami_numlibros   = case_when(
      fami_numlibros == "0 A 10 LIBROS"     ~ 0,
      fami_numlibros == "11 A 25 LIBROS"    ~ 1,
      fami_numlibros == "26 A 100 LIBROS"   ~ 2,
      fami_numlibros == "MÁS DE 100 LIBROS" ~ 3,
      TRUE ~ as.numeric(fami_numlibros)
    ),
    fami_estrato = case_when(
      fami_estratovivienda == "Sin Estrato" ~ 0,
      fami_estratovivienda == "Estrato 1" ~ 1,
      fami_estratovivienda == "Estrato 2" ~ 2,
      fami_estratovivienda == "Estrato 3" ~ 3,
      fami_estratovivienda == "Estrato 4" ~ 4,
      fami_estratovivienda == "Estrato 5" ~ 5,
      fami_estratovivienda == "Estrato 6" ~ 6,
      TRUE ~ as.numeric(fami_estratovivienda)
    )
  ) %>%
  select(-fami_estratovivienda)

# PIB por departamento
pib_df <- read_xlsx("DANE - PIB.xlsx", sheet = "Cuadro 3", range = "A10:T43") %>%
  select(1, 20) %>%
  setNames(c("estu_cod_reside_depto", "PIB")) %>%
  mutate(estu_cod_reside_depto = as.integer(estu_cod_reside_depto))
datos <- datos %>%
  left_join(pib_df, by = "estu_cod_reside_depto") %>%
  filter(!is.na(PIB) & estu_cod_reside_depto != 88)

#--- 3. Construcción de matrices de diseño --------------------------------
# Estudiante (X)
est <- datos %>% transmute(
  dept_id  = estu_cod_reside_depto,
  muni_id  = estu_cod_reside_mcpio,
  y         = punt_global,
  etnia     = as.numeric(estu_tieneetnia),
  madre     = as.numeric(fami_educacionmadre),
  estrato   = as.numeric(fami_estrato),
  libros    = as.numeric(fami_numlibros),
  compu     = as.numeric(fami_tienecomputador),
  internet  = as.numeric(fami_tieneinternet)
)
# Eliminar casos con NA en X o y
est <- est %>% filter(complete.cases(est))

X_mat   <- as.matrix(est %>% select(etnia, madre, estrato, libros, compu, internet))
y_vec   <- est$y
muni_id <- est$muni_id
dept_id <- est$dept_id

# Municipio (Z)
mun <- datos %>%
  group_by(dept_id = estu_cod_reside_depto, muni_id = estu_cod_reside_mcpio) %>%
  summarise(
    nbi          = first(nbi),
    doc_por_est  = first(docentotal/alumntotal)*100,
    tasa_homic   = first(homicidios/pobl_tot)*100000,
    tasa_dezplaz = first(desplazados_expulsion/pobl_tot)*100000,
    risk_victim  = first(RISK_VICTIM_2022),
    media_mun    = mean(punt_global),
    .groups = "drop"
  ) %>% filter(complete.cases(.))
Z_mat <- as.matrix(mun %>% select(nbi, doc_por_est, tasa_homic, tasa_dezplaz, risk_victim))

# Departamento (W)
depto <- datos %>%
  group_by(dept_id = estu_cod_reside_depto) %>%
  summarise(
    pib          = first(PIB) / 1e6,
    pobl_rur     = first((pobl_rur / pobl_tot) * 100),
    tasa_homic   = first(homicidios/pobl_tot)*100000,
    tasa_dezplaz = first(desplazados_expulsion/pobl_tot)*100000,
    riesgo       = first(porcentaje_en_riesgo),
    .groups = "drop"
  ) %>% filter(complete.cases(.))
W_mat <- as.matrix(depto %>% select(pib, pobl_rur, tasa_dezplaz, riesgo, tasa_homic))

# Expandir por estudiante e inspeccionar NAs
Z_big <- Z_mat[match(muni_id, mun$muni_id), ]
W_big <- W_mat[match(dept_id, depto$dept_id), ]
stopifnot(!anyNA(Z_big), !anyNA(W_big))

#--- 3.5 Estandarizar matrices de diseño ----------------------------------
# Centrar y escalar para estabilidad numérica
X_mat <- scale(X_mat)
Z_big <- scale(Z_big)
W_big <- scale(W_big)

#--- 4. Pre-cálculos fuera de MCMC ----------------------------------------
XtX_ind <- crossprod(X_mat)
ZtZ     <- crossprod(Z_big)
WtW     <- crossprod(W_big)

# Dimensiones y priors
B <- 100
p <- ncol(X_mat); q <- ncol(Z_big); r <- ncol(W_big)
M <- nrow(mun); D <- nrow(depto)
mu0 <- 250; sigma0_prior <- 10
muE <- rep(0,p); sigmaE_prior <- 10
muM <- rep(0,q); sigmaM_prior <- 10
muD <- rep(0,r); sigmaD_prior <- 10
nu_kappa <- 1

#--- 5. Inicialización -----------------------------------------------------
beta0  <- mu0
betaE  <- muE; betaM <- muM; betaD <- muD
sigma0 <- sigma0_prior; sigmaE <- sigmaE_prior
sigmaM <- sigmaM_prior; sigmaD <- sigmaD_prior
kappa_j <- rep(1,M); kappa_k <- rep(1,D)
alpha_k <- 1; beta_k <- 1
chain <- matrix(NA, B, 1+p+q+r+M+D+6)
log_lik <- numeric(B)  # Almacenará la log-verosimilitud

#--- 5.1 Funciones Gibbs robustas -------------------------------------------
update_beta0 <- function(mu, var_inv, y, mu_pred, w) {
  inv_sum <- var_inv + sum(w, na.rm = TRUE)
  if (!is.finite(inv_sum) || inv_sum <= 0) {
    inv_sum <- var_inv + length(w) * mean(w, na.rm = TRUE)
  }
  V <- 1 / inv_sum
  m_comp <- mu * var_inv + sum(w * (y - mu_pred), na.rm = TRUE)
  m <- V * m_comp
  if (!is.finite(V) || !is.finite(m)) {
    warning("Non-finite V or m in update_beta0; keeping previous mu value")
    return(mu)
  }
  rnorm(1, m, sqrt(V))
}

update_beta_vec <- function(XtX, XtY, pri_inv, mu, jitter = 1e-5) {
  post_prec <- pri_inv + XtX
  # Añadir jitter y usar pseudoinversa si es necesario
  V <- tryCatch(
    solve(post_prec + diag(jitter, nrow(post_prec))), 
    error = function(e) {
      warning("Matriz no invertible, usando pseudoinversa con jitter adicional")
      solve(post_prec + diag(10*jitter, nrow(post_prec)))
    }
  )
  m <- V %*% (XtY + pri_inv %*% mu)
  
  # Verificar finitud antes de muestrear
  if (any(is.na(m)) || any(!is.finite(m))) {
    warning("Valores no finitos en m, usando valores anteriores")
    return(mu)
  }
  
  # Muestrear con Cholesky (más estable)
  drop(rmvnorm(1, m, V, method = "chol"))
}

update_kappa_j <- function(n_j, ssr_j, nu, parent_kappa) {
  shape <- 0.5 * (nu + n_j)
  rate  <- 0.5 * (nu * parent_kappa + ssr_j)
  
  # Manejar casos problemáticos
  if (is.na(rate) || rate <= 0 || !is.finite(rate)) {
    warning(paste("Tasa inválida en kappa_j: n_j=", n_j, "ssr_j=", ssr_j, 
                  "parent=", parent_kappa, "usando 1e-6"))
    rate <- 1e-6
  }
  1 / rgamma(1, shape, rate = rate)
}

update_kappa_k <- function(n_k, sum_inv_kappa_j, nu, parent_alpha) {
  shape <- 0.5 * (nu + n_k)
  rate  <- 0.5 * (nu * parent_alpha + sum_inv_kappa_j)
  
  # Manejar casos problemáticos
  if (is.na(rate) || rate <= 0 || !is.finite(rate)) {
    warning(paste("Tasa inválida en kappa_k: n_k=", n_k, "sum_inv=", sum_inv_kappa_j, 
                  "parent=", parent_alpha, "usando 1e-6"))
    rate <- 1e-6
  }
  1 / rgamma(1, shape, rate = rate)
}

#--- 5.2 Función de log-verosimilitud --------------------------------------
# Basada en la distribución muestral: y ~ N(ζ, κ_j²)
# Donde ζ = β0 + x'βE + z'βM + w'βD
compute_log_lik <- function(y, X, Z, W, beta0, betaE, betaM, betaD, kappa_j, muni_id) {
  # Calcular media ζ para cada observación
  zeta <- beta0 + X %*% betaE + Z %*% betaM + W %*% betaD
  
  # Obtener varianzas para cada observación según su municipio
  var_obs <- kappa_j[muni_id]
  
  # Calcular log-verosimilitud para cada observación
  log_lik_i <- dnorm(y, mean = zeta, sd = sqrt(var_obs), log = TRUE)
  
  # Sumar todas las contribuciones
  sum(log_lik_i)
}

#--- 6. Sampler Gibbs con manejo robusto de errores ------------------------
tic()
for(iter in 1:B) {
  tryCatch({
    #--- 6.1 Predicción y pesos ---
    mu_pred <- beta0 + X_mat %*% betaE + Z_big %*% betaM + W_big %*% betaD
    w_inv   <- 1 / kappa_j[muni_id]
    w_inv   <- pmin(w_inv, 1e6)  # Limitar pesos inversos
    
    #--- 6.2 Actualizar parámetros ---
    beta0  <- update_beta0(mu0, 1/sigma0, y_vec, mu_pred, w_inv)
    
    # Coeficientes E
    resid  <- y_vec - beta0 - Z_big %*% betaM - W_big %*% betaD
    Xw     <- sqrt(w_inv) * X_mat
    betaE  <- update_beta_vec(crossprod(Xw), crossprod(Xw, resid), diag(1/sigmaE, p), muE)
    
    # Coeficientes M
    resid  <- y_vec - beta0 - X_mat %*% betaE - W_big %*% betaD
    Zw     <- sqrt(w_inv) * Z_big
    betaM  <- update_beta_vec(crossprod(Zw), crossprod(Zw, resid), diag(1/sigmaM, q), muM)
    
    # Coeficientes D
    resid  <- y_vec - beta0 - X_mat %*% betaE - Z_big %*% betaM
    Ww     <- sqrt(w_inv) * W_big
    betaD  <- update_beta_vec(crossprod(Ww), crossprod(Ww, resid), diag(1/sigmaD, r), muD)
    
    #--- 6.3 Varianzas municipales (kappa_j) ---
    for(j in 1:M) {
      idx <- which(muni_id == mun$muni_id[j])
      if (length(idx) > 0) {
        # Calcular SSR para el municipio j
        mu_j <- beta0 + X_mat[idx, , drop = FALSE] %*% betaE + 
          Z_big[idx, , drop = FALSE] %*% betaM + 
          W_big[idx, , drop = FALSE] %*% betaD
        
        ssr <- sum((y_vec[idx] - mu_j)^2)
        parent_kappa <- kappa_k[unique(dept_id[idx])[1]]
        kappa_j[j] <- update_kappa_j(length(idx), ssr, nu_kappa, parent_kappa)
      } else {
        warning(paste("Municipio", mun$muni_id[j], "sin observaciones. Manteniendo kappa anterior."))
      }
    }
    
    #--- 6.4 Varianzas departamentales (kappa_k) ---
    for(k in 1:D) {
      dept_id_k <- depto$dept_id[k]
      municipios_en_depto <- mun$muni_id[mun$dept_id == dept_id_k]
      
      if (length(municipios_en_depto) > 0) {
        # Encontrar índices de kappa_j para estos municipios
        j_indices <- match(municipios_en_depto, mun$muni_id)
        inv_kappa_sum <- sum(1 / kappa_j[j_indices])
        kappa_k[k] <- update_kappa_k(length(municipios_en_depto), inv_kappa_sum, nu_kappa, alpha_k)
      } else {
        warning(paste("Departamento", dept_id_k, "sin municipios. Manteniendo kappa anterior."))
      }
    }
    
    #--- 6.5 Hiperparámetros de varianza ---
    sigma0 <- 1/rgamma(1, (2)/2, (sigma0_prior + (beta0 - mu0)^2)/2)
    sigmaE <- 1/rgamma(1, (p+1)/2, (sigmaE_prior*p + sum(betaE^2))/2)
    sigmaM <- 1/rgamma(1, (q+1)/2, (sigmaM_prior*q + sum(betaM^2))/2)
    sigmaD <- 1/rgamma(1, (r+1)/2, (sigmaD_prior*r + sum(betaD^2))/2)
    
    #--- 6.6 Parámetros jerárquicos ---
    # Prevenir log(0) con pmax
    log_kappa_k <- log(pmax(kappa_k, 1e-100))
    alpha_k <- rgamma(1, 1 + 0.5*D*nu_kappa, 1 + 0.5*sum(log_kappa_k))
    beta_k  <- rgamma(1, 1 + 0.5*D*alpha_k, 1 + 0.5*sum(kappa_k))
    
    #--- 6.7 Calcular log-verosimilitud ---
    log_lik[iter] <- compute_log_lik(
      y = y_vec,
      X = X_mat,
      Z = Z_big,
      W = W_big,
      beta0 = beta0,
      betaE = betaE,
      betaM = betaM,
      betaD = betaD,
      kappa_j = kappa_j,
      muni_id = muni_id
    )
    
    #--- 6.8 Guardar muestra ---
    chain[iter, ] <- c(beta0, betaE, betaM, betaD, kappa_j, kappa_k, 
                       sigma0, sigmaE, sigmaM, sigmaD, alpha_k, beta_k)
    
    #--- 6.9 Mostrar progreso ---
    if (iter %% 50 == 0) {
      cat(sprintf("Iteración %d/%d completada | beta0: %.2f | LogLik: %.2f | alpha_k: %.4f | beta_k: %.4f\n",
                  iter, B, beta0, log_lik[iter], alpha_k, beta_k))
    }
  }, error = function(e) {
    warning(paste("Error en iteración", iter, ":", e$message))
    # Mantener valores anteriores en caso de error
    if (iter > 1) {
      chain[iter, ] <<- chain[iter-1, ]
      log_lik[iter] <<- log_lik[iter-1]
    }
  })
}
toc()

#--- 7. Guardar resultados -------------------------------------------------
save(chain, log_lik, file = "gibbs_chain_optimized_final.RData")
cat("MCMC completado exitosamente. Cadena y log-verosimilitud guardadas.\n")

#--- 8. Información de sesión ----------------------------------------------
sessionInfo()
