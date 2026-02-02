library(glmtoolbox)
library(performance)
library(MASS)
library(vcd)
# 1
dt = MASS::quine

set.seed(123)
dt = dt[sample(1:nrow(dt), size = nrow(dt)),]
head(dt)

dt.train = dt[1:ceiling(nrow(dt)*.7),]
dt.test = dt[ceiling(nrow(dt)*.7 + 1):nrow(dt),]

# A

# modelo poisson
fit.pois = glm(Days ~ Lrn + Age + Sex + Eth, family = poisson(log), data = dt.train)
check_model(fit.pois)
check_zeroinflation(fit.pois)

# modelo binomial negativo
fit.nbinom = glm.nb(Days ~ Age + Eth, data = dt.train)
check_model(fit.nbinom)
check_zeroinflation(fit.nbinom)

# ambos modelos tienen cero inflado
# no explican bien la dispersion
# los datos no se ajustan a las predicciones

AIC(fit.nbinom);AIC(fit.pois)
BIC(fit.nbinom);BIC(fit.pois)

# el modelo binomial negativo explica mejor los datos (criterio AIC)

# el RMSE es la media de las desviaciones cuadradas entre prediccion y datos observados 

cat(" Pois - RMSE", sqrt(mean((dt.train$Days - fit.pois$fitted.values)^2)),"\n")
cat("Binom - MAE", sqrt(mean((dt.train$Days - fit.nbinom$fitted.values)^2)),"\n")

# el RMSE es la media de las desviaciones absolutas entre prediccion y datos observados

cat(" Pois - MAE", mean(abs(dt.train$Days - fit.pois$fitted.values)),"\n")
cat("Binom - MAE", mean(abs(dt.train$Days - fit.nbinom$fitted.values)),"\n")

# dan valores muy altos para ambos modelos

# B

cat(" Pois - RMSE", sqrt(mean((dt.test$Days - predict(fit.pois, newdata = dt.test, type = "response"))^2)),"\n")
cat("Binom - RMSE", sqrt(mean((dt.test$Days - predict(fit.nbinom, newdata = dt.test, type = "response"))^2)),"\n")
cat(" Pois - MAE", mean(abs(dt.test$Days - predict(fit.pois, newdata = dt.test, type = "response"))),"\n")
cat("Binom - MAE", mean(abs(dt.test$Days - predict(fit.nbinom, newdata = dt.test, type = "response"))),"\n")

# valores PEORES
# no toca estimar AIC ni BIC porque ambas medidas implican el uso de verosimilitud, algo que no se puede calcular
# para estos datos porque no hacen parte de las estimaciones de la misma

# C

rootogram(x = dt.train$Days, fitted = fit.pois$fitted.values)
rootogram(x = dt.train$Days, fitted = fit.nbinom$fitted.values)

# hay subestimacion 

# 2

library(pscl)

dt = pscl::bioChemists
# A 

fit.pois = zeroinfl(art ~ fem + mar + kid5 + phd + ment,
                      data = dt, dist = "poisson")
summary(fit.pois)
AIC(fit.pois)
BIC(fit.pois)

fit.nbin = zeroinfl(art ~ fem + mar + kid5 + phd + ment,
                      data = dt, dist = "negbin")
summary(fit.nbin)
AIC(fit.nbin)
BIC(fit.nbin)
# ambos modelos seC1alan que ment es una variable significativa para crear ceros inflados

fit.pois.2 = zeroinfl(art ~ fem + mar + kid5 + phd + ment | ment,
                      data = dt, dist = "poisson")
check_model(fit.pois.2)
AIC(fit.pois.2)
BIC(fit.pois.2)
fit.nbin.2 = zeroinfl(art ~ fem + mar + kid5 + phd + ment | ment,
                      data = dt, dist = "negbin")
check_model(fit.nbin.2)
AIC(fit.nbin.2)
BIC(fit.nbin.2)

# B
# es pertinente realizar este tipo de comparaciones ya que a pesar de modelar
# el zero inflado (y seguramente agregarle complejidad al modelo, aumentando el valor de ambos criterios)
# la verosimilitud indica quC) tan bien explica el modelo a los datos, 
# sin importar la funcion de distribucion usada

# C

vuong(fit.nbin.2, fit.pois.2)
vuong(fit.nbin, fit.nbin.2)

# ya se tiene que ajustando el cero inflado por med, agregamos otros dos covariantes

fit.nbin.3 = zeroinfl(art ~ fem + mar + kid5 + phd + ment | ment + mar,
                      data = dt, dist = "negbin")
fit.nbin.4 = zeroinfl(art ~ fem + mar + kid5 + phd + ment | ment + kid5,
                      data = dt, dist = "negbin")
fit.nbin.5 = zeroinfl(art ~ fem + mar + kid5 + phd + ment | ment + kid5 + mar,
                      data = dt, dist = "negbin")

vuong(fit.nbin.2, fit.nbin.3)
vuong(fit.nbin.2, fit.nbin.4)
vuong(fit.nbin.2, fit.nbin.5)

fit.nbin.2.b = zeroinfl(art ~ fem + mar + kid5 + ment | ment,
                      data = dt, dist = "negbin")
fit.nbin.2.c = zeroinfl(art ~ fem + kid5 + ment | ment,
                        data = dt, dist = "negbin")
vuong(fit.nbin.2, fit.nbin.2.b) # comparamos con un modelo mas simple
vuong(fit.nbin.2, fit.nbin.2.c) # comparamos con un modelo mas simple
vuong(fit.nbin.2.c, fit.nbin.2.b) # comparamos con un modelo mas simple
# el modelo sin phd como variable explicativa pero con ment explicando es el "mejor"

check_model(fit.nbin.2.b)

# D

summary(fit.nbin.2.b)

exp(fit.nbin.2.b$coefficients$count)-1
# las mujeres relacionadas en la variable "fem" reducen un 19% el conteo de artC-culos
# estar casado aparentemente aumenta en un 15% el conteo de artC-culos, falta mas datos para verificar
# por cada hijo menor a 5 reduce en un 15.4% la cantidad de artC-culos escritos
# por cada ment aumenta un 2.4% el nC:mero de artC-culos escritos
# existe sobredispersion significativa en el modelo

exp(fit.nbin.2.b$coefficients$zero)-1
# aumentos por unidades de ment reducen un 45% el chance de NO escribir artC-culos

# E

# puede ser porque aumentos en esta variable reducen el chance de hallar ceros
# y a su vez aumentan el conteo. En este caso, se tiene que recibir mentorC-as
# reduce el chance de no presentar artC-culos, lo que serC-a equivalente a comentar que
# recibirlas aumenta la cantidad de artC-culos escritos

# 3

# A
dt = MASS::quine
library(gamlss)  

library(GGally)
PLOT = ggpairs(dt)

fit.gamlss.negbin.zinf = gamlss(Days ~ Lrn + Age + Sex + Eth, family = ZINBI(mu.link = "log"), data = dt)
check_model(fit.gamlss.negbin.zinf)

fit.gamlss.negbin = gamlss(Days ~ Lrn + Age + Sex + Eth, family = NBI(mu.link = "log"), data = dt)
check_model(fit.gamlss.negbin)


# poisson no sirvieron

# que cosa tan mala xd revision con NB

library(DHARMa)
library(psych)

pairs.panels(dt)

fit.nbinom = glm.nb(Days ~ Age + Eth, data = dt)
check_model(fit.nbinom, residual_type = "normal")
summary(fit.nbinom)

head(table(dt$Days,dt$Eth), n = 1)
head(table(dt$Days,dt$Sex), n = 1)
head(table(dt$Days,dt$Age), n = 1)
head(table(dt$Days,dt$Lrn), n = 1)

# la C:nica que parece explicarme el cero inflado es Eth y tal vez Sex

fit.zinbinom.gamlss = gamlss(Days ~ Age + Eth,
                           family = ZIP(mu.link = "log", nu.link = "logit"),
                           data = dt,
                           sigma.formula = ~ Eth + Lrn + Age,
                           nu.formula = ~ Eth + Sex)

check_model(fit.zinbinom.gamlss, residual_type = "normal")


gamlss.quine = fit.zinbinom.gamlss


pred.g.quine = predict(gamlss.quine, data = dt, type = "response")

cat("NBIN - RMSE", sqrt(mean((dt$Days - pred.g.quine)^2)),"\n")
cat("NBIN - MAE ", mean(abs(dt$Days - pred.g.quine)),"\n")

# igual de mala a la prediccion original



cat("Binom - RMSE", sqrt(mean((dt$Days - fit.nbinom$fitted.values)^2)),"\n")
cat("Binom - MAE", mean(abs(dt$Days - fit.nbinom$fitted.values)),"\n")

# esta estimando mejor para valores altos que el modelo glm.nb
# exploicar la varianza bajo Lrn y Sex ; donde ser SL en lrn aumenta un 6% la variaciC3n y ser hombre aumenta un 1.5% la variaciC3n de los dC-as contados

# 4

# Par??metros
n <- 1000     # n??mero de conteos
pi <- 0.3     # probabilidad de cero estructural
mu <- 2       # media del componente Poisson

# Generaci??n de la mezcla
set.seed(123)
u <- runif(n)
y <- ifelse(u < pi, 0, rpois(n, mu))

# Resultado
table(y)
mean(y == 0)  # proporci??n de ceros observada


