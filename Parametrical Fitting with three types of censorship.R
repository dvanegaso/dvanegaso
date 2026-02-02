library(survival)

# Creamos ejemplo con los cuatro tipos de censura
# ------------------------------------------------
# id : tipo
# 1-2  → observación exacta
# 3    → censura a izquierda
# 4-5  → censura a derecha
# 6-7  → censura por intervalo

L <- c(5,  #  NO censura
       10, #  NO censura
       0,   # censura a izquierda
       20,
       40,
       30,
       50)
R <- c(5,  #  NO censura
       10, #  NO censura
       8,
       Inf, # censura a derecha
       Inf, # censura a derecha
       35, # censura a intervalo
       60) # censura a intervalo

# No censura: Colocar en L y R los mismos valores.
# Censura a Derecha: Colocar el valor en L e inf en R.
# Censura a Izquierda: Colocar en 0 en L y el valor en R.
# Censura a Intervalo: Colocar el intervalo en L y R.

# Construcción del objeto de supervivencia general
T_all <- Surv(L, R, type = "interval2")           # NA es "fleming-harrington" (sobrevivencia)
T_all                                             # KM es "kaplan-meier"       (riesgo acumulado)

sobre.fit = survfit(T_all ~ 1)

par(mfrow = c(2,2))

plot((summary(sobre.fit)$time),(summary(sobre.fit)$surv), type = "l", main = "Exponencial")
plot(log(summary(sobre.fit)$time),log(summary(survfit(T_all ~ 1))$surv), type = "l", main = "Weibull")
plot(qnorm(1-exp(-summary(sobre.fit)$time)),log(summary(sobre.fit)$surv), type = "l", main = "LogNormal")
plot(log(exp(summary(sobre.fit)$time)-1),log(summary(sobre.fit)$surv), type = "l", main = "log-Logística")

par(mfrow = c(1,1))

Y.d = c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161) # intervalo a derecha
d = c(0,0,1,0,0,1,0,0,1,0,1) # indicador de SI censura a derecha
l = c(0,0,0,0,0,0,0,0,0,0,0) # indicador de SI censura a izquierda
i = c(0,0,0,0,0,0,0,0,0,0,0) # indicador de SI censura a ambos lados
Y.l = c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161) # intervalo a izquierda

Y.d = c(0.5, 1, 0.75, 0.25, 1.25)
d = c(0,0,0,0,0)
l = c(0,0,0,1,1)
d = c(0,0,0,0,0)
Y.d = c(0.5, 1, 0.75, 0.25, 1.25)


#####################################
#### Distribuciones Paramétricas ####
#####################################

# Exponencial

llexp <- function(b){
  lambda <- b[1]   # parámetro de tasa
  
  # Protección de dominio
  if (any(!is.finite(lambda)) || is.na(lambda) || lambda <= 0)
    return(1e10)
  
  # Cálculos básicos
  Fd <- 1 - exp(-lambda * Y.d)
  Fl <- 1 - exp(-lambda * Y.l)
  
  diff <- pmax(Fd - Fl, 1e-12)  # evitar log(0)
  
  loglik <- sum(
    (1 - d) * (1 - l) * (1 - i) * log(dexp(Y.d, rate = lambda)) +  # exactos
      d * (1 - l) * (1 - i) * (-lambda * Y.d) +                      # censura derecha
      (1 - d) * l * (1 - i) * log(Fl) +                              # censura izquierda
      i * log(diff)                                                  # intervalo
  )
  
  if (!is.finite(loglik) || is.na(loglik)) return(1e10)
  
  return(-loglik)
}

b0 <- c(0.01)
Par.Exp <- nlminb(b0, llexp, lower = 1e-6, upper = 2)$par
Par.Exp

# Weibull

llwei <- function(b){
  k <- b[1]        
  lambda <- b[2]   
  
  # protección total de dominio
  if (any(!is.finite(b)) || any(is.na(b)) || k <= 0 || lambda <= 0) 
    return(1e10)
  
  scale <- lambda^(-1/k)
  
  Fd <- pweibull(Y.d, shape = k, scale = scale, lower.tail = TRUE)
  Fl <- pweibull(Y.l, shape = k, scale = scale, lower.tail = TRUE)
  
  diff <- pmax(Fd - Fl, 1e-12)  # evitar log(0)
  
  loglik <- sum(
    (1 - d) * (1 - l) * (1 - i) * log(dweibull(Y.d, shape = k, scale = scale)) +  # exactos
      d * (1 - l) * (1 - i) * log(1 - Fd) +                                         # censura derecha
      (1 - d) * l * (1 - i) * log(Fl) +                                             # censura izquierda
      i * log(diff)                                                                 # intervalo
  )
  
  if (!is.finite(loglik) || is.na(loglik)) return(1e10)
  
  return(-loglik)
}

b0 <- c(1, 0.01)
Par.W <- nlminb(b0, llwei, lower = c(1e-6, 1e-6), upper = c(5, 2))$par
Par.W

# Log-Normal

lllogn <- function(b){
  mu <- b[1]
  sigma <- b[2]
  
  # Protección del dominio
  if (any(!is.finite(b)) || any(is.na(b)) || sigma <= 0)
    return(1e10)
  
  Fd <- plnorm(Y.d, meanlog = mu, sdlog = sigma, lower.tail = TRUE)
  Fl <- plnorm(Y.l, meanlog = mu, sdlog = sigma, lower.tail = TRUE)
  
  diff <- pmax(Fd - Fl, 1e-12)  # evita log(0)
  
  loglik <- sum(
    (1 - d) * (1 - l) * (1 - i) * log(dlnorm(Y.d, meanlog = mu, sdlog = sigma)) +  # exactos
      d * (1 - l) * (1 - i) * log(1 - Fd) +                                          # censura derecha
      (1 - d) * l * (1 - i) * log(Fl) +                                              # censura izquierda
      i * log(diff)                                                                  # intervalo
  )
  
  if (!is.finite(loglik) || is.na(loglik)) return(1e10)
  
  return(-loglik)
}

b0 <- c(log(mean(Y.d)), 1)
Par.LN <- nlminb(b0, lllogn, lower = c(-10, 1e-6), upper = c(10, 5))$par
Par.LN

# Log-Logística

llloglogis <- function(b){
  alpha <- b[1]  # escala
  beta  <- b[2]  # forma
  
  if (any(!is.finite(b)) || any(is.na(b)) || alpha <= 0 || beta <= 0)
    return(1e10)
  
  # Funciones auxiliares
  f_loglogis <- function(t) (beta/alpha) * (t/alpha)^(beta - 1) / (1 + (t/alpha)^beta)^2
  F_loglogis <- function(t) 1 / (1 + (alpha/t)^beta)
  
  Fd <- F_loglogis(Y.d)
  Fl <- F_loglogis(Y.l)
  diff <- pmax(Fd - Fl, 1e-12)
  
  loglik <- sum(
    (1 - d) * (1 - l) * (1 - i) * log(f_loglogis(Y.d)) +   # exactos
      d * (1 - l) * (1 - i) * log(1 - Fd) +                  # censura derecha
      (1 - d) * l * (1 - i) * log(Fl) +                      # censura izquierda
      i * log(diff)                                          # censura intervalo
  )
  
  if (!is.finite(loglik) || is.na(loglik)) return(1e10)
  
  return(-loglik)
}

b0 <- c(median(Y.d), 1)
Par.LL <- nlminb(b0, llloglogis, lower = c(1e-6, 1e-6), upper = c(1e5, 10))$par
Par.LL

# Graficación

t <- seq(0.01, 2, length.out = 1000)  # empieza en >0 para evitar log(0)

# --- Exponencial ---
lambda <- Par.Exp[1]
h.exp <- rep(lambda, length(t))

# --- Weibull ---
k <- Par.W[1]
lambda <- Par.W[2]
scale <- lambda^(-1 / k)
h.weib <- (k / scale) * (t / scale)^(k - 1)

# --- Lognormal ---
mu <- Par.LN[1]
sigma <- Par.LN[2]
f.ln <- dlnorm(t, meanlog = mu, sdlog = sigma)
S.ln <- plnorm(t, meanlog = mu, sdlog = sigma, lower.tail = FALSE)
h.ln <- f.ln / S.ln

# --- Loglogística ---
alpha <- Par.LL[1]
beta <- Par.LL[2]
f.ll <- (alpha / beta) * (t / beta)^(alpha - 1) / (1 + (t / beta)^alpha)^2
S.ll <- 1 / (1 + (t / beta)^alpha)
h.ll <- f.ll / S.ll

# --- Gráfico combinado ---
plot(t, h.exp, type = "l", lwd = 2, col = "blue",
     ylim = c(0, max(h.weib, h.ln, h.ll, na.rm = TRUE)),
     xlab = "Tiempo", ylab = "h(t)",
     main = "Funciones de riesgo (hazard) teóricas")

lines(t, h.weib, col = "red", lwd = 2, lty = 2)
lines(t, h.ln, col = "darkgreen", lwd = 2, lty = 3)
lines(t, h.ll, col = "purple", lwd = 2, lty = 4)

legend("topright", legend = c("Exponencial", "Weibull", "Lognormal", "Loglogística"),
       col = c("blue", "red", "darkgreen", "purple"),
       lwd = 2, lty = 1:4, cex = 0.8, bty = "n")
grid()
