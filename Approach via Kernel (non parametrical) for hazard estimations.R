

alpha_E <- function(q) {
  numerador <- 64 * (2 - 4*q + 6*q^2 - 3*q^3)
  denominador <- (1 + q)^4 * (19 - 18*q + 3*q^2)
  return(numerador / denominador)
}

beta_E <- function(q) {
  numerador <- 240 * (1 - q)^2
  denominador <- (1 + q)^4 * (19 - 18*q + 3*q^2)
  return(numerador / denominador)
}

alpha_BW <- function(q) {
  numerador <- 64 * (8 - 24*q + 48*q^2 - 45*q^3 + 15*q^4)
  denominador <- (1 + q)^5 * (81 - 168*q + 126*q^2 - 40*q^3 + 5*q^4)
  return(numerador / denominador)
}

beta_BW <- function(q) {
  numerador <- 1120 * (1 - q)^3
  denominador <- (1 + q)^5 * (81 - 168*q + 126*q^2 - 40*q^3 + 5*q^4)
  return(numerador / denominador)
}

K_ep.S.a = function(x,q) {  # kernel epanechnikov asimétrico para h(t)
  ifelse(abs(x) < 1, 0.75 * (1 - x^2) * (alpha_E(q) + beta_E(q)*x ), 0)
}


K_bw.S.a = function(x,q) {
  ifelse(abs(x) < 1, (15/16) * (1 - x^2)^2 * (alpha_BW(q) + beta_BW(q)*x ), 0)
}

K_ep.S = function(x) {
  ifelse(abs(x) < 1, 0.75 * (1 - x^2), 0)
}

K_bw.S = function(x) {
  ifelse(abs(x) < 1, (15/16) * (1 - x^2)^2, 0)
}

# recordar que
#------------------------------------------------------------------|
# caso              | estimador                  | uso del kernel  |
#------------------------------------------------------------------|
# t < h             | q = t / h                  | x =   x         |
#------------------------------------------------------------------|
# h < t < t_r - h   | estimador kernel simétrico |                 |
#------------------------------------------------------------------|
# t_r - h < t < t_r | q = (t_r - t) / h          | x = - x         |
#------------------------------------------------------------------|

# InformaciC3n muestral ( Y , delta)
Y2<-c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45)
d2<-c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)


# InformaciC3n unificada del tiempo al evento 

library(survival)
T2 = Surv(Y2,d2);T2

# NA es "fleming-harrington" (sobrevivencia)
# KM es "kaplan-meier"       (riesgo acumulado)

t_i = as.numeric(names(table(T2))); t_i = t_i[!is.na(t_i)]

# CC!lculo de estimaciones con instrucciC3n "survfit"
Sur.NA2 = survfit( T2 ~ 1, type="fleming-harrington", error = "t", conf.type="plain" )
summary(Sur.NA2)

Hhat = -log(Sur.NA2$surv)
dH2 = diff(c(0, Hhat))

t_r = max(t_i)

dH2 = dH2[-length(dH2)]

kernel <- function(t, h) {
  if (t < h) {
    kernel_val <- sum(K_ep.S.a(x = (t - t_i) / h, q = t / h) * dH2)
  } else if (t >= h & t <= t_r - h) {
    kernel_val <- sum(K_ep.S(x = (t - t_i) / h) * dH2)
  } else {
    kernel_val <- sum(K_ep.S.a(x = - (t - t_i) / h, q = (t_r - t) / h) * dH2)
  }
  return(kernel_val / h)
}


kernel(t = 190, h = 18)

x = seq(from = 0, to = t_r, length.out = 1000)

y = NULL

for (i in 1:length(x)) {
  y[i] = kernel(t = x[i], h = 5)
}

plot(x, y, type = "l")



