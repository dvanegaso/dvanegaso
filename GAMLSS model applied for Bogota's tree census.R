
# All data previously cleaned for this models.

# Evauación bases de datos

##############################
########## Árboles ###########
##############################

# haré una muestra de tamaño n = 500 de arboles

set.seed(123)
datos_Arbol = datos_Arbol[sample(nrow(datos_Arbol)),]
# datos Arbol sample
dt.A.s = datos_Arbol[1:500]

summary(datos_Arbol)
summary(dt.A.s) # me gusta esta muestra, se asemejan

pairs.panels(dt.A.s)

colnames(dt.A.s) 

# altura muy correlacionada con fisiologia, diametro, perimetro, polar, y geolocalizacion
# tratare de hacer un gamlss con la gamma generalizada (4p)

formula.1 = altura_tot ~ diam_ecuat + perim_basa + diam_polar
# oooO Max Verstappen
  
m.1 = gamlss(formula = formula.1,
             family = JSU,
             data = dt.A.s,
             control = gamlss.control(n.cyc = 200))
# convergió

plot(m.1)

summary(m.1)

ajuste = function(estimados, reales){
  cat("RMSE",sqrt(mean((reales - estimados)^2)),"\n")
  cat(" MAE",(mean(abs(reales - estimados))),"\n")
}

# medidas de estimacion
ajuste(fitted.values(m.1), dt.A.s$altura_tot); ajuste(predict(m.1, newdata = datos_Arbol, type = "response"), datos_Arbol$altura_tot)

m.1.2 = gamlss(formula = altura_tot ~ diam_ecuat + perim_basa + diam_polar
               ,family = JSU, sigma.formula = ~ transparen + formatronc  + densidad + ang_inclin,
               data = dt.A.s,
               control = gamlss.control(n.cyc = 2000))
#ctoc::toc()

#LR.test(m.1,m.1.2)

# Es significativamente mejor un modelo JSU donde la varianza sea explciada por
# el diametro polar

summary(m.1.2)
plot(m.1.2)

ajuste(fitted.values(m.1.2), dt.A.s$altura_tot); ajuste(predict(m.1.2, newdata = datos_Arbol, type = "response"), datos_Arbol$altura_tot)

##############################
########## Arbustos ##########
##############################



##############################
########### Palmas ###########
##############################


##############################
########## General ###########
##############################


set.seed(123)
datos = datos[sample(nrow(datos)),]
# datos Arbol sample
dt.s = datos[1:1000]

summary(datos_Arbol)
summary(dt.s) # me gusta esta muestra, se asemejan

pairs.panels(dt.s)

colnames(dt.s) 

# altura muy correlacionada con fisiologia, diametro, perimetro, polar, y geolocalizacion
# tratare de hacer un gamlss con la gamma generalizada (4p)

formula.1 = altura_tot ~ diam_ecuat + perim_basa + diam_polar + tipo_arbol
# oooO Max Verstappen

par(mfrow = c(3,1))
plot(datos$diam_ecuat, datos$altura_tot)
plot(datos$diam_polar, datos$altura_tot)
plot(datos$perim_basa, datos$altura_tot)
par(mfrow = c(1,1))

m.1 = gamlss(formula = formula.1,
             family = JSU,
             data = dt.s,
             control = gamlss.control(n.cyc = 200))
plot(m.1)

summary(m.1)

ajuste = function(estimados, reales){
  cat("RMSE",sqrt(mean((reales - estimados)^2)),"\n")
  cat(" MAE",(mean(abs(reales - estimados))),"\n")
}

# medidas de estimacion
ajuste(fitted.values(m.1), dt.s$altura_tot); ajuste(predict(m.1, newdata = datos, type = "response"), datos$altura_tot)

