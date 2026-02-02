rm(list = ls())
library(survival)
setwd("")

###################
##### crédito #####
###################

dt.credito = read.table("", header = TRUE)


# Objeto Surv
y1 <- dt.credito$t 
D1 <- dt.credito$dD # entrar en default
y <- Surv(y1,D1)
# Covariantes
Sex = dt.credito$sexo
ED = dt.credito$edad

# Ajuste del modelo log logistico
m.vida.acel.1.llgis = survreg(y ~ Sex + ED ,dist = "loglogistic") 
summary(m.vida.acel.1.llgis) # solo se muestra significativa la edad
exp(coefficients(m.vida.acel.1.llgis)) # por cada año que pasa, el tiempo hasta que la persona entre en default aumenta en un 2.5%


# Ajuste del modelo de cox
m.cox = coxph(y ~ Sex + ED) 
summary(m.cox)
exp(coefficients(m.cox))
pred = survfit(m.cox, newdata = data.frame(ED = 40, Sex = 1))

pred$surv[length(pred$surv)-3]

summary(pred, times = 25)$surv
summary(pred, times = 24)$surv

# por cada año que pasa, el riesgo de que la persona entre en default se reduce en un 3.44% 
# no se muestran diferencias significativas en entradas de default según el género

###################
##### cáncer ######
################### 

rm(list = ls())
library(survival)

Y1<-c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161) 
d1<-c(1,1,0,1,1,0,1,1,0,1,0)
Y2<-c(5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45)
d2<-c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

Y = c(Y1, Y2)
d = c(d1,d2)
s = c(rep("control", length(Y1)), rep("esperar", length(Y2))) # 0 hombres, 1 terapia

T_cancer = Surv(Y,d)
T_cancer

# verificar si la reincidencia de cancer es igual bajo aplicar terapia activa de quimio o no aplicarla

m.1 = survreg(T_cancer ~ s,dist="loglogistic")
summary(m.1) 
exp(coefficients(m.1))

# una terapia activa de quimio aumenta el tiempo de reaparición de cáncer en un 83%

m.2 = coxph(T_cancer ~ s)
summary(m.2)

sf <- survfit(m.2, newdata = data.frame(s = "control"))
sf$surv


exp(coefficients(m.2))

# aplicar una terapia activa de quimio reduce el riesgo de reaparición de cáncer en un 60%

# Interpretación: las mujeres tienen 59% menos riesgo de desarrollar cancer de pulmón

# hallar la probabilidad de que el paciente recaiga en cancer dado que t>12 (ambos grupos)

#################################
##### distribucion loglogis #####
#################################

surv.logis = function(t, sigma, mu, gamma, Z){
  # paso a distribución logística
  alpha = 1/sigma
  rho = exp(-mu/sigma)
  beta = -gamma/sigma
  
  # cálculo de funcion de sobrevivencia
  
  return(1/(1 + (rho * (t^alpha) * exp(t(beta)%*%Z))))
}

dens.logis = function(t, sigma, mu, gamma, Z){
  # paso a distribución logística
  alpha = 1/sigma
  rho = exp(-mu/sigma)
  beta = -gamma/sigma
  
  # cálculo de funcion de densidad
  
  return((rho * alpha * (t^alpha) * exp(t(beta)%*%Z))/((1 + (rho * (t^alpha) * exp(t(beta)%*%Z)))^2))
}

################################
##### distribucion weibull #####
################################

surv.weibull = function(t, sigma, mu, gamma, Z){
  # paso a distribución weibull
  alpha = 1/sigma
  rho = exp(-mu/sigma)
  beta = -gamma/sigma
  
  # cálculo de funcion de sobrevivencia
  
  return(exp( - rho * (t^alpha) * exp(t(beta)%*%Z)))
}

################################
####### modelamiento cox #######
################################


