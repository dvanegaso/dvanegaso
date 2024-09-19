library(plotly)
library(summarytools)
library(knitr)
library(MASS)

#-------------------------------------------------------------------------------
#Tratamiento de datos

setwd("C:/Users/thetr/OneDrive/Escritorio/Parcial Inferencia III")

creditos<-read.delim("creditos.txt")

creditos <- creditos[,c("INGRESOS_DECLARADOS_TOTA", "SEXO", "MONTO_TOTAL_OTORGADO", "NIVEL_ESTUDIOS", "ESTRATOS")]
# renombrar variables
colnames(creditos) <- c("ingresos", "genero", "monto", "estudios", "estrato")
# redefinir escala de los ingresos para trabajar en millones
creditos$ingresos <- creditos$ingresos/1000000
creditos$monto <- creditos$monto/1000000

# filtrar por ingresos inferiores a 100M y crédito hipotecario o de vehículo
creditos <- creditos[(creditos$monto < 100) & (creditos$ingresos<25),]
#-------------------------------------------------------------------------------
#Reclasificación y ajustamiento de unidades

creditos$genero<-as.factor(creditos$genero)

creditos$estudios<-as.factor(creditos$estudios)

creditos$estrato<-as.factor(creditos$estrato)


#-------------------------------------------------------------------------------
#clasificación según genero

creditos_hom<-creditos[creditos$genero == "H",]
#view(t(descr(creditos_hom)))
creditos_muj<-creditos[creditos$genero == "M",]
#view(t(descr(creditos_muj)))
#-------------------------------------------------------------------------------
#separación de ingresos

ingr_hom<-creditos_hom$ingresos

ingr_muj<-creditos_muj$ingresos

ingr_hom<-as.data.frame(ingr_hom)

ingr_muj<-as.data.frame(ingr_muj)

#-------------------------------------------------------------------------------
#histogramas y boxplot

# histograma
fig_hist_hom <- plot_ly(ingr_hom, x=~ingr_hom, type="histogram",colors = "blue",nbinsx = 49)
fig_hist_hom <- fig_hist_hom %>% layout(title = "Ingresos",
                                        yaxis=list(title='Frecuencia'))

# boxplot
fig_box_hom <- plot_ly(ingr_hom, x=~ingr_hom, type="box",colors = "blue",color = "blue")
fig_box_hom <- fig_box_hom %>% layout(title = "Ingresos Hombres",
                                      yaxis=list(title='Frecuencia'))

# pegamos ambos gráficos
fig_ingr_hom <- subplot(fig_hist_hom, fig_box_hom, nrows = 1, shareX = TRUE, titleX = TRUE)

fig_ingr_hom

# histograma
fig_hist_muj <- plot_ly(ingr_muj, x=~ingr_muj, type="histogram",color ="red",nbinsx = 49)
fig_hist_muj <- fig_hist_muj %>% layout(title = "Ingresos",
                                        yaxis=list(title='Frecuencia'))

# boxplot
fig_box_muj <- plot_ly(ingr_muj, x=~ingr_muj, type="box",colors = "red",color = "red")
fig_box_muj <- fig_box_muj %>% layout(title = "Ingresos Mujeres",
                                      yaxis=list(title='Frecuencia'))

# pegamos ambos gráficos
fig_ingr_muj <- subplot(fig_hist_muj, fig_box_muj, nrows = 1)

fig_ingr_muj
#-------------------------------------------------------------------------------
#Tabulación 
tab_exp_ingr_hom<-t(descr(ingr_hom))
tab_exp_ingr_muj<-t(descr(ingr_muj))

tab_exp_ingr<-rbind(tab_exp_ingr_hom,tab_exp_ingr_muj)
tab_exp_ingr<-as.data.frame(tab_exp_ingr)
tab_exp_ingr <- tab_exp_ingr[, !(names(tab_exp_ingr) %in% c("MAD","Pct.Valid","N.Valid","Kurtosis","SE.Skewness","Skewness","IQR"))]

colnames(tab_exp_ingr)<-c("Media","Desv.Est.","Mín.","Cuartil 1","Mediana","Cuartil 3","Máx.","Coef.Var(%)")
rownames(tab_exp_ingr)<-c("Hombres","Mujeres")

kable(tab_exp_ingr,format = "latex",digits = 2)
ingr_hom<-creditos_hom$ingresos

ingr_muj<-creditos_muj$ingresos

#-------------------------------------------------------------------------------
#------------------------histogramas y ajustes----------------------------------
#-------------------------------------------------------------------------------
#usamos plotly pero al usar histogramas con más de 40 particiones se dañaban los
#gráficos, por lo que decidimos fijarlos a 30 con fines de presentación
#histograma hombres
ajuste_logn_hom <- fitdistr(ingr_hom, "lognormal")

x <- seq(min(ingr_hom), max(ingr_hom), length = 1000)

d_log_hom <- dlnorm(x, meanlog = ajuste_logn_hom$estimate['meanlog'], sdlog = ajuste_logn_hom$estimate['sdlog'])

fig <- plot_ly(x = ingr_hom, type = "histogram", histnorm = "probability", nbinsx = 49, name = "Datos")

fig <- fig %>% add_trace(x = x, y = d_log_hom, type = "scatter", mode = "lines", line = list(color = 'blue'), name = "Modelo Lognormal")

fig <- fig %>% layout(title = "Histograma y Modelo Lognormal - Hombres",
                      xaxis = list(title = "Valores (Millones de Pesos)"),
                      yaxis = list(title = "Densidad"))

fig
#-------------------------------------------------------------------------------
#histograma mujeres
ajuste_logn_muj <- fitdistr(ingr_muj, "lognormal")

x <- seq(min(ingr_muj), max(ingr_muj), length = 1000)

d_log_muj <- dlnorm(x, meanlog = ajuste_logn_muj$estimate['meanlog'], sdlog = ajuste_logn_muj$estimate['sdlog'])

fig <- plot_ly(x = ingr_muj, type = "histogram", histnorm = "probability", name = "Datos",nbinsx = 49, color = "red")

fig <- fig %>% add_trace(x = x, y = d_log_muj, type = "scatter", mode = "lines", line = list(color = 'red'), name = "Modelo Lognormal")

fig <- fig %>% layout(title = "Histograma y Modelo Lognormal - Mujeres",
                      xaxis = list(title = "Valores (Millones de Pesos)"),
                      yaxis = list(title = "Densidad"))

fig
ingr_hom<-creditos_hom$ingresos

ingr_muj<-creditos_muj$ingresos

ingr_hom<-as.matrix(ingr_hom)
ingr_muj<-as.matrix(ingr_muj)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# AJUSTE LOGNORMAL
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------



ajuste_log_hom<-fitdistr(ingr_hom, "lognormal")
ajuste_log_muj<-fitdistr(ingr_muj, "lognormal")


est_log_hom<-ajuste_log_hom$estimate
est_log_muj<-ajuste_log_muj$estimate
# lo se pero es que esta cosa no botaba vectores XD
# ese yerry mina es un bobazo
# pasamos los valores de la inferencia a datos que podamos usar :v

meanlog_hom_MLE<-est_log_hom[1]
meanlog_muj_MLE<-est_log_muj[1]

sdlog_hom_MLE<-est_log_hom[2]
sdlog_muj_MLE<-est_log_muj[2]

varlog_hom_MLE<-sdlog_hom_MLE^2
varlog_muj_MLE<-sdlog_muj_MLE^2
#calculamos el MLE del valor esperado de cada población usando el principio de invarianza
expect_log_hom_MLE<-exp(meanlog_hom_MLE+((varlog_hom_MLE)/2))
expect_log_muj_MLE<-exp(meanlog_muj_MLE+((varlog_muj_MLE)/2))
#calculamos el MLE de la diferencia entre ambos valores esperados
dif_expect_hom_muj_MLE<-expect_log_hom_MLE-expect_log_muj_MLE

#ahora se viene lo bueno >>>:DD necesitaré fleshgod apocalypse pa sacar esta cosa a toda XD

#-------------------------------------------------------------------------------

#calculamos la varianza de cada estimador usando la fórmula obtenida en la sección 4.1 del documento de entrega

n_hombres<-dim(ingr_hom)[1]
n_mujeres<-dim(ingr_muj)[1]

# ahora si la varianza de cada uno >:D

var_expect_log_hom_MLE<-(1/n_hombres)*((varlog_hom_MLE)*(expect_log_hom_MLE^2)+(1/2)*(varlog_hom_MLE^2)*(expect_log_hom_MLE^2))
var_expect_log_muj_MLE<-(1/n_mujeres)*((varlog_muj_MLE)*(expect_log_muj_MLE^2)+(1/2)*(varlog_muj_MLE^2)*(expect_log_muj_MLE^2))
var_dif_expect_hom_muj_MLE<-sqrt(var_expect_log_hom_MLE+var_expect_log_muj_MLE)

IC_095_dif_hom_muj_log<-dif_expect_hom_muj_MLE+c(-1,1)*qnorm(p = 0.975)*var_dif_expect_hom_muj_MLE
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# AJUSTE NORMAL
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#si el nomrbe de las variables están raras es porque usé este código para el punto 29 del sexto taller  
mes_despues<-ingr_hom
siete_meses<-ingr_muj
#comprobamos heterocedasticidad
sdcompus<-c(sd(mes_despues),sd(siete_meses))
n_primer_mes<-n_hombres
n_septimo_mes<-n_mujeres
#así calculamos el valor de v:
v<-round(((var(mes_despues)/n_primer_mes+var(siete_meses)/n_septimo_mes)^2)/(((var(mes_despues)/n_primer_mes)^2/(n_primer_mes-1))+((var(siete_meses)/n_septimo_mes)^2/(n_septimo_mes-1))))
#acá es claro que este v nos da lo mismo para una Z
#luego hallamos el IC:
var_dif_expect_hom_muj_normal<-sqrt((var(mes_despues)/n_primer_mes)+(var(siete_meses)/n_septimo_mes))
margen_error_compus<-c(-qt(p = 0.975,df = v)*var_dif_expect_hom_muj_normal,qt(p = 0.975,df = v)*var_dif_expect_hom_muj_normal)
dif_expect_hom_muj_normal<-mean(mes_despues)-mean(siete_meses)
IC_095_dif_hom_muj_normal<-dif_expect_hom_muj_normal+margen_error_compus
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Bootstrap
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#hombres
#por precisión este pedazo se tardará arto tiempo (~30seg en un portatil potente), dado que usamos 10000 remuestreos en ambos

M <- 10000
xb_boot_h <- matrix(data = NA, nrow = M, ncol = 1)
set.seed(2103)
for (i in 1:M) {
  xr_h <- sample(x = ingr_hom, size = n_hombres, replace = TRUE)
  xb_boot_h[i] <- mean(xr_h)
}

#mujeres

xb_boot_m <- matrix(data = NA, nrow = M, ncol = 1)
set.seed(2103)
for (i in 1:M) {
  xr_h <- sample(x = ingr_muj, size = n_mujeres, replace = TRUE)
  xb_boot_m[i] <- mean(xr_h)
}
# A ver si sirve este machetazo xddd
sd_boots_dif<-sqrt(var(xb_boot_h)+var(xb_boot_m))
xb_boot_dif<-xb_boot_h-xb_boot_m
IC_095_dif_hom_muj_quartile_bootstrap<-quantile(xb_boot_dif, probs = c(0.025,0.975))
IC_095_dif_hom_muj_quartile_bootstrap<-as.data.frame(IC_095_dif_hom_muj_quartile_bootstrap)[,1]
IC_095_dif_hom_muj_empiric_bootstrap<-2*(mean(xb_boot_dif)) - quantile(xb_boot_dif, probs = c(0.975,0.025))
IC_095_dif_hom_muj_empiric_bootstrap<-as.data.frame(IC_095_dif_hom_muj_empiric_bootstrap)[,1]
IC_095_dif_hom_muj_normal_bootstrap<-(mean(xb_boot_dif)) + c(-1,1)*qnorm(p = 0.975)*sd(xb_boot_dif)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#construcción de esa tabla xd

#coeficientes de variación
cv_log<-var_dif_expect_hom_muj_MLE/dif_expect_hom_muj_MLE
cv_norm<-var_dif_expect_hom_muj_normal/dif_expect_hom_muj_normal
cv_bp<-sd_boots_dif/mean(xb_boot_dif)
cv_be<-sd_boots_dif/((mean(xb_boot_dif)))
cv_bn<-sd_boots_dif/(mean(xb_boot_dif))

#márgenes de error
ME_log<-IC_095_dif_hom_muj_log[2]-dif_expect_hom_muj_MLE
ME_norm<-IC_095_dif_hom_muj_normal[2]-dif_expect_hom_muj_normal
ME_bp<-IC_095_dif_hom_muj_quartile_bootstrap[2]-median(xb_boot_dif)
ME_be<-IC_095_dif_hom_muj_empiric_bootstrap[2]-mean(xb_boot_dif)
ME_bn<-IC_095_dif_hom_muj_normal_bootstrap[2]-mean(xb_boot_dif)

tabla <- rbind(
  c(dif_expect_hom_muj_MLE,var_dif_expect_hom_muj_MLE,cv_log,ME_log,unname(IC_095_dif_hom_muj_log)), 
  c(dif_expect_hom_muj_normal,var_dif_expect_hom_muj_normal,cv_norm,ME_norm,unname(IC_095_dif_hom_muj_normal)), 
  c(median(xb_boot_dif),sd_boots_dif,cv_bp,ME_bp,unname(IC_095_dif_hom_muj_quartile_bootstrap)), 
  c(2*(mean(xb_boot_dif)),sd_boots_dif,cv_be,ME_be,unname(IC_095_dif_hom_muj_empiric_bootstrap)), 
  c(mean(xb_boot_dif),sd_boots_dif,cv_bn,ME_bn,unname(IC_095_dif_hom_muj_normal_bootstrap))
)

colnames(tabla)<-c("Estimación","Error Estándar","Coef. de Var (%)","Márgen de Error","IC Inferior","IC Superior")
rownames(tabla)<-c("Ajuste Lognormal","Ajuste Normal","Bootstrap Percentiles","Bootstrap Empírico","Bootstrap Normal")
kable(tabla,format = "latex",digits = 3)
print(tabla)