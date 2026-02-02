library(performance)
library(glmtoolbox)

cook_graph = function(object){
  # NO FUNCIONA CON GAMLSS
  # calculo de distancias de cook
  p = length(coefficients(object))
  cook = cooks.distance(object); n = length(cook)
  
  # graficación
  plot(cook, main = "Datos Influyentes", type = "h", ylab = "Distancia de Cook", xlab = "")
  abline(h = 4/(n-p-1), col = "red")
  legend("topleft",legend = c("Cook"," 4/(n-p-1)"), col = c("black","red"), lty = 1, lwd = 2)
  
  influent = which(cook > 4/(n-p-1))
  
  # conteo de datos influyentes
  cat("There's a total of",length(influent),"influential data:","\n")
  
  return(influent)
}

comp.table = function(...){
  # tabla comparativa entre modelos (devianza y aic)
  modelos = list(...)
  k = length(modelos)
  
  calls = as.character(match.call())[-1]
  
  # medidas
  dev.m = sapply(modelos, deviance)
  aic.m = sapply(modelos, AIC)
  
  table = rbind(devianza = dev.m,
                aic = aic.m)
  
  colnames(table) = calls
  return(knitr::kable(table))
  }

###############################
########### PUNTO 1 ###########
###############################

dt1 = data.frame(sexo = c(rep(x = "Mujer", times = 12),
                         rep(x = "Hombre",times = 12)),
                edad = c(40,36,40,38,42,39,40,37,36,38,39,40,
                         40,38,40,35,36,37,41,40,37,38,40,38),
                peso = c(3317,2729,2935,2754,
                         3210,2817,3126,2539,
                         2412,2991,2875,3231,
                         2968,2795,3163,2925,
                         2625,2847,3292,3473,
                         2628,3176,3421,2975))
m.1 = glm(peso ~ edad*sexo, family = gaussian(link = "identity"), data = dt1)
summary(m.1)
# no hay interaccion entre sexo y edad, por lo que no existe evidencia estadísticamente significativa indicando que
# el peso del recien nacido es diferente entre niños y niñas.

check_model(m.1) # hay una multicolinealidad severa




# los residuales no parecen seguir una distribucion normal, procedemos
# por lo que las inferencias de este modelo son completamente inválidas

inf = cook_graph(m.1)

# esa observacion en efecto es influyente. relizando otro modelo sin esa observacion:

m.1.1 = glm(peso ~ edad*sexo, family = gaussian(link = "identity"), data = dt1[-inf,])
summary(m.1.1)
check_model(m.1.1) # multicolinealidad considerable 
shapiro.test(residuals(m.1.1)) # siguen sin ser normales los residuales, inferencias inválidas
check_distribution(m.1.1)
envelope(m.1.1) # tambien se muestra un mal modelo

cook_graph(m.1.1) # removidos datos influyentes.

comp.table(m.1,m.1.1) # notablemente mejor que el modelo con dato influyente

tabla = matrix(c(m.1$coefficients,m.1.1$coefficients),
               ncol = length(m.1$coefficients),
               nrow = 2,
               byrow = T)

tabla = data.frame(tabla)
colnames(tabla) = c("Intercepto","Edad","SexoMujer","edad:sexoMujer")
rownames(tabla) = c("con influyente","sin influyente")

knitr::kable(tabla) # note que al remover el dato influyente el factor edad aumenta
# en 48 gramos de peso

# interpretaciónd e parámetros (sin observacion influyente)
# bajo un modelo normal, por cada semana de gestación, el peso al nacer aumenta
# en paroximadamente 112 gramos, esto con una significancia de $\alpha = 0.05$

# Y con otros coeficientes no significativos para este enfoque:
# - los hombres recien nacidos nacen con aproximadamente 603.59 gramos de mas sobre las recien nacidas
# - para una recien nacida, por cada semana que pase de gestación va a pesar 19.15  gramos menos que 
# un hombre recien nacido y gestado en el mismo intervalo de tiempo

p.1 = data.frame(edad = 40,
                sexo = "Mujer")

predict(m.1.1, newdata = p.1)
predict(m.1, newdata = p.1)


###############################
########### PUNTO 2 ###########
###############################

dt2 = data.frame(escala = c(9,13,6,8,10,4,14,8,11,7,9,7,
                            5,14,13,16,10,12,11,14,15,18,
                            7,16,9,9,11,13,15,13,10,11,6,
                            17,14,19,9,11,14,10,16,16,14,
                            13,13,9,15,10,11,12,4,14,20,10),
                 sens = c(rep(1,times = 14),rep(0, times = 40)))

m.2.logit = glm(sens ~ escala, family = binomial(link = "logit"), data = dt2)
m.2.probit = glm(sens ~ escala, family = binomial(link = "probit"), data = dt2)
m.2.loglog = glm(sens ~ escala, family = binomial(link = "cloglog"), data = dt2)

comp.table(m.2.logit,m.2.loglog,m.2.probit) # voy a trabajar con el modelo probit

summary(m.2.probit) # por cada aumento en unidad de la escala de wechler, se reducen las 
# probabilidades de presentar síntomas de demencia en 0.19 desviaciones estandar respecto
# a la media poblacional

# en español, por cada unidad aumentada en ese test, la probabilidad de tener senilidad
cat("se reduce en",pnorm(-0.18801),"respecto a la unidad anterior","\n") 

check_model(m.2.probit)
check_distribution(m.2.probit) # parece realziarse una buena elección sobre la distribucion
envelope(m.2.probit) # no hay sospechas de sobredispersion

inf = cook_graph(m.2.probit) # nos encontramos con cuatro observaciones influyentes

p.2 = data.frame(escala = c(15))
predict(m.2.probit, newdata = p.2) # la probabilidad de tener senilidad con ese puntaje se 
# reduce en 1.434065 desviaciones estandar de la media, es decir:
cat("la probabilidad de tener senilidad es:",pnorm(-1.434065), "\n")


###############################
########### PUNTO 3 ###########
###############################

dt3 = data.frame(dosis = c(0.41,0.58,0.71,0.89,1.01),
                 exp = c(50,48,46,49,50),
                 muertos = c(6,16,24,42,44))
dt3$survivors = dt3$exp - dt3$muertos
dt3$tasa = dt3$muertos/dt3$exp
plot(dt3$dosis,dt3$tasa, xlim = c(0,1),ylim = c(0,1), ylab = "Tasa de Muertes", xlab = "Dosis") 
# a mayor dosis en efecto se presenta mayor mortalidad
# aplicar dosis mayores a 0.89 parece ser innecesario, no hay una diferencia significativa 
# respecto a aplicar 1.01 de dosis
# desconsiderando 1.01 de dosis, se presenta un comportamiento lineal creciente segun la dosis

# toca pasar los datos a formato plano


m.3.logit = glm(cbind(muertos, survivors) ~ dosis, family = binomial(link = "logit"), data = dt3)
m.3.loglog = glm(cbind(muertos, survivors) ~ dosis, family = binomial(link = "cloglog"), data = dt3)
m.3.probit = glm(cbind(muertos, survivors) ~ dosis, family = binomial(link = "probit"), data = dt3)

comp.table(m.3.probit, m.3.loglog, m.3.logit)

# tocará usar entonces el modelo  logit, tiene una devianza considerablemente mejor

m.3.logit0 = glm(cbind(muertos, survivors) ~ 1, family = binomial(link = "logit"), data = dt3)
# las mejores metricas las da el modelo logit, es acorde al grafico envelope y al
# check model

anova(m.3.logit0,m.3.logit) # existe evidencia estadísticamente suficiente para 
# considedar que la dosis afecta 

envelope(m.3.logit) # el modelo ajusta incorrectamente 
check_model(m.3.logit) # no se predice correctamente para dosis de 0.89
infl.m.3.logit = cook_graph(m.3.logit) # 19 observaciones influyentes

# las interpretaciones del modelo cambian radicalmente (se duplica el coeficiente asociado)
# al no tenerse en cuenta todas las observaciones consideradas como influyentes

p.3 = data.frame(dosis = c(0.5))
exp(predict(m.3.logit, newdata = p.3))-1  # se reducen las chances 


###############################
########### PUNTO 4 ###########
###############################

dt4.liver = data.frame(dosis = c(0,0.3,0.35,0.45,0.6,0.75,1,1.5),
                       exp = c(555,2014,1102,550,441,382,213,211),
                       enf = c(6,34,20,15,13,17,19,24))

dt4.liver$tasa = dt4.liver$enf/dt4.liver$exp
plot(dt4.liver$dosis, dt4.liver$tasa)

dt4.liver$sano = dt4.liver$exp - dt4.liver$enf

dt4.bladder = data.frame(dosis = c(0,0.3,0.35,0.45,0.6,0.75,1,1.5),
                       exp = c(101,443,200,103,66,75,31,11),
                       enf = c(1,5,0,2,2,12,21,11))

dt4.bladder$tasa = dt4.bladder$enf/dt4.bladder$exp
plot(dt4.bladder$dosis, dt4.bladder$tasa)

dt4.bladder$sano = dt4.bladder$exp - dt4.bladder$enf

m.4.liver.logis = glm(cbind(enf,sano) ~ dosis, family = binomial(link = "logit"), data = dt4.liver)
m.4.bladder.logis = glm(cbind(enf,sano) ~ dosis, family = binomial(link = "logit"), data = dt4.bladder)
m.4.liver.pois = glm(enf ~ dosis, family = poisson(link = "log"), data = dt4.liver)
m.4.bladder.pois = glm(enf ~ dosis, family = poisson(link = "log"), data = dt4.bladder)

# selección de modelos higado

comp.table(m.4.bladder.logis, 
           m.4.bladder.pois)
comp.table(m.4.liver.logis,
           m.4.liver.pois)

# los modelos basados en logístico tienen mejor AIC

summary(m.4.liver.logis)
summary(m.4.liver.pois)
summary(m.4.bladder.logis)
summary(m.4.bladder.pois)
# modelos de tipo logístico tienen mejores inferencias

par(mfrow = c(2,2))
envelope(m.4.liver.logis, main = "Logístico Hígado")
envelope(m.4.bladder.logis, main = "Logístico Vejiga")
par(mfrow = c(1,1))
# modelos de tipo poisson no se ven bien ajustados
# me quedaré con modelos logísticos en este caso

# ajusto modelos nulos de tipo logístico:
m.4.liver.logis0 = glm(cbind(enf,sano) ~ 1, family = binomial(link = "logit"), data = dt4.liver)
m.4.bladder.logis0 = glm(cbind(enf,sano) ~ 1, family = binomial(link = "logit"), data = dt4.bladder)

anova(m.4.liver.logis0,m.4.liver.logis)
anova(m.4.bladder.logis0, m.4.bladder.logis)
# ambos se muestran significativos bajo las estadísticas de wald

check_model(m.4.liver.logis)
envelope(m.4.liver.logis)
inf.4.liver = cook_graph(m.4.liver.logis) # conteo extremadamente influenciante
m.4.liver.logis.1 = glm(cbind(enf,sano) ~ dosis, family = binomial(link = "logit"), data = dt4.liver[-8,])

check_model(m.4.liver.logis.1)# mejoran las estimaciones del modelo al corregirse
cook_graph(m.4.liver.logis.1)# mejoran las estimaciones del modelo al corregirse

knitr::kable(cbind(c("con influenciante", "sin influenciante"),rbind(m.4.liver.logis$coefficients, m.4.liver.logis.1$coefficients)))

# cambian mucho las interpretaciones de las dosis

###############################
########### PUNTO 5 ###########
###############################

dt5 = data.frame(t1 = c(33.3,52.2,64.7,137,125.9,116.3,131.7,85,91.9),
                 t2 = c(25.3,14.4,32.5,20.5,97.6,53.6,56.6,87.3,47.8),
                 fallas = c(15,9,14,24,27,27,23,18,22)) 
m.5.logis = glm(fallas ~ t1 + t2, family = poisson(link = "log"), data = dt5)
m.5.ident = glm(fallas ~ t1 + t2, family = poisson(link = "identity"), data = dt5)
m.5.sqrt = glm(fallas ~ t1 + t2, family = poisson(link = "sqrt"), data = dt5)

comp.table(m.5.logis, m.5.ident, m.5.sqrt)
# me quedo con el modelo logit (mucho mas interpretable que raiz cuadrada)

m.5.logis0 = glm(fallas ~ 1, family = poisson(link = "log"), data = dt5)
m.5.logis1 = glm(fallas ~ t1, family = poisson(link = "log"), data = dt5)
m.5.logis2 = glm(fallas ~ t2, family = poisson(link = "log"), data = dt5)

anova(m.5.logis0, m.5.logis1)
anova(m.5.logis0, m.5.logis2)
# esta raro, ambos tiempos son significativos en un modelo

anova(m.5.logis1, m.5.logis) # un modelo agregando el t2 no es significativamente mejor
# explicando los datos que el coso

# envelope no funciona
check_model(m.5.logis1) # varianza mal especificada para valores mayores a 22
cook_graph(m.5.logis1) # cero datos influyentes

exp(coefficients(m.5.logis1)[2]) -1 # por cada segundo aumentado, aumenta un 0.77% la cantidad de fallas de  las piezas

###############################
########### PUNTO 6 ###########
###############################

dt6 = data.frame(t = c(0,15,30,45,60),
                 lesiones = c(271, 108, 59,29,12))
plot(dt6$t, dt6$lesiones) # se ven inversamente proporcionales

m.6.log = glm(lesiones ~ t, family = poisson(link = "log"), data = dt6)
m.6.ident = glm(lesiones ~ t, family = poisson(link = "identity"), data = dt6)
m.6.sqrt = glm(lesiones ~ t, family = poisson(link = "sqrt"), data = dt6)

comp.table(m.6.log, m.6.sqrt, m.6.ident) # me quedo con el modelo de enlace logarítmico

m.6.log0 = glm(lesiones ~ 1, family = poisson(link = "log"), data = dt6)

anova(m.6.log0, m.6.log) # se muestra significativamente mejor un ajuste de este tipo al nulo

check_model(m.6.log)
envelope(m.6.log)
cook_graph(m.6.log)

summary(m.6.log)

p.6 = data.frame(t = c(30))
(predict(m.6.log, newdata = p.6))  

# Se esperan al menos 55 lesiones.

###############################
########### PUNTO 7 ###########
###############################

dt7 = data.frame(ciudad = c(rep("st paul", times = 7),rep("fort worth", times = 7)),
                 edad = c("15-24","25-34","35-44","45-54","55-64","65-74","75+",
                          "15-24","25-34","35-44","45-54","55-64","65-74","75+"),
                 casos = c(1,16,30,71,102,130,40,4,38,119,221,259,310,65),
                 poblacion = c(172675,123065,96216,92051,72159,54722,8328,
                               181343,146207,121374,111353,83004,55932,7583))

m.7 = glm(casos ~ ciudad + edad, family = poisson(link = "log"), data = dt7, offset = log(poblacion))

st = m.7$coefficients

m.7 = glm(casos ~ ciudad + edad,
          offset = log(poblacion), # tomé en cuenta el tamaño de la población
          family = poisson(link = "log"),
          data = dt7)

m.70 = glm(casos ~ 1, family = poisson(link = "log"), data = dt7)

anova(m.70, m.7) # es significativamente mejor explicando este modelo que el nulo
summary(m.7) # significativamente dependiente de la ciudad

check_model(m.7)
cook_graph(m.7) 

# en efecto depende de la ciudad

exp(m.7$coefficients[2]) -1

# es decir que se reduce en un 57% la incidencia de cancer de piel en san pablo sobre fort worth

p.7 = data.frame(edad = c("75+"), ciudad = "st paul", poblacion = 10000)
(predict(m.7, newdata = p.7))/10000

###############################
########### PUNTO 8 ###########
###############################

dt8 = faraway::wafer
m.8.id = glm(resist ~ x1 + x2+ x3 +x4, family = Gamma(link = "identity"), data = dt8)
m.8.log = glm(resist ~ x1 + x2+ x3 +x4, family = Gamma(link = "log"), data = dt8)
m.8.inv = glm(resist ~ x1 + x2+ x3 +x4, family = Gamma(link = "inverse"), data = dt8)

check_model(m.8.id)
check_model(m.8.log)
check_model(m.8.inv)  # mejor homogeneidad de la varianza en el enlace inverso
# el envelope tambien se meuestra mejor >:v

tabla = matrix(c(deviance(m.8.id),deviance(m.8.log),deviance(m.8.inv),
                 AIC(m.8.id),AIC(m.8.log),AIC(m.8.inv)),ncol = 3, byrow = T)
tabla = data.frame(tabla)
colnames(tabla) = c("log","identidad", "inverso")
rownames(tabla) = c("devianza","AIC")
knitr::kable(tabla)
# sería una bunea idea pegarnos al tercer modelo si eso fuera interpretable >:v

summary(m.8.inv)
# solo el primer y tercer factor son significativos

cook_graph(m.8.inv) # no valores influyentes, de pura chiripa no sirve >:v

