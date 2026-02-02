# Integrantes: 
# Jorge Andres Sanchez Duarte
# Daniel Mauricio Vanegas Oliveros

# Notas:
# - Approaches made: - log(m)
#                    - logit(q)
#                    - log(q)
# - Para los modelos ARIMA se eligió el que diera mejores residuales.
# - El vector kappa fué multiplicado por -1 ya que mejoraba laes estimaciones.

# - Al final del documento se encuentran las tres tablas


 
rm(list = ls())

library(forecast)

# All previous mortality data was collected by DANE

m = data.frame(
  Edad = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
           "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
           "60 a 64","65 a 69","70 a 74","75 a 79","80 +"),
  H_1985_1990 = c(0.04945,0.00468,0.00065,0.00067,0.00179,0.00386,0.00466,
                  0.00498,0.00419,0.00472,0.00670,0.00906,0.01533,0.02257,
                  0.03259,0.04818,0.07359,0.13149),
  M_1985_1990 = c(0.03966,0.00420,0.00048,0.00039,0.00075,0.00096,0.00120,
                  0.00151,0.00185,0.00276,0.00408,0.00625,0.00814,0.01832,
                  0.02425,0.03726,0.05976,0.11037),
  
  H_1990_1995 = c(0.03759, 0.00319, 0.00054, 0.00065, 0.00342, 0.00582, 0.00578,
                  0.00536, 0.00566, 0.00562, 0.00608, 0.00831, 0.01273, 0.01805,
                  0.02948, 0.04338, 0.06655, 0.12943),
  M_1990_1995 = c(0.02886, 0.00282, 0.00039, 0.00039, 0.00083, 0.00097, 0.00110,
                  0.00131, 0.00176, 0.00242, 0.00360, 0.00571, 0.00943, 0.01409,
                  0.02310, 0.03650, 0.05846, 0.10727),
  
  H_1995_2000 = c(0.03362,0.00232,0.00053,0.00064,0.00290,0.00512,0.00509,0.00474,
                  0.00495,0.00497,0.00551,0.00774,0.01135,0.01672,0.02679,0.04006,
                  0.06353,0.13232),
  M_1995_2000 = c(0.02434, 0.00196, 0.00036, 0.00037, 0.00073, 
                  0.00093, 0.00099, 0.00118, 0.00159, 0.00216, 
                  0.00328, 0.00508, 0.00793, 0.01218, 0.01992, 
                  0.03189, 0.05102, 0.11226),
  
  H_2000_2005 = c(0.02923, 0.00156, 0.00051, 0.00063, 0.00236,
                  0.00436, 0.00435, 0.00406, 0.00419, 0.00427,
                  0.00488, 0.00709, 0.00984, 0.01519, 0.02379,
                  0.03630, 0.05997, 0.13613),
  M_2000_2005 = c(0.02046, 0.00136, 0.00032, 0.00035, 0.00063,
                  0.00090, 0.00088, 0.00106, 0.00143, 0.00193,
                  0.00298, 0.00452, 0.00666, 0.01050, 0.01715,
                  0.02784, 0.04453, 0.11784),
  
  H_2005_2010 = c(0.02411, 0.00122, 0.00042, 0.00051, 0.00185,
                  0.00336, 0.00336, 0.00320, 0.00339, 0.00365,
                  0.00444, 0.00657, 0.00949, 0.01485, 0.02359,
                  0.03674, 0.06041, 0.13566),
  M_2005_2010 = c(0.01613, 0.00113, 0.00028, 0.00030, 0.00054,
                  0.00076, 0.00076, 0.00092, 0.00126, 0.00172,
                  0.00268, 0.00408, 0.00605, 0.00958, 0.01573,
                  0.02585, 0.04223, 0.11639),
  
  H_2010_2015 = c(0.02097, 0.00092, 0.00034, 0.00040, 0.00140,
                  0.00248, 0.00249, 0.00244, 0.00269, 0.00310,
                  0.00405, 0.00612, 0.00918, 0.01456, 0.02342,
                  0.03713, 0.06079, 0.13533),
  M_2010_2015 = c(0.01362, 0.00092, 0.00024, 0.00025, 0.00046,
                  0.00064, 0.00064, 0.00079, 0.00109, 0.00153,
                  0.00239, 0.00366, 0.00548, 0.00871, 0.01440,
                  0.02399, 0.04008, 0.11520),
  
  H_2015_2020 = c(0.01863, 0.00072, 0.00029, 0.00033, 0.00108,
                  0.00187, 0.00188, 0.00191, 0.00221, 0.00272,
                  0.00377, 0.00581, 0.00896, 0.01435, 0.02330,
                  0.03741, 0.06106, 0.13509),
  M_2015_2020 = c(0.01185, 0.00074, 0.00020, 0.00021, 0.00038,
                  0.00053, 0.00055, 0.00068, 0.00096, 0.00136,
                  0.00215, 0.00331, 0.00501, 0.00799, 0.01330,
                  0.02246, 0.03831, 0.11424))

#############################################
###### estimación por medio de m (log) ######
#############################################

m.hombres = m[,c(rep(1:7)*2)]
m.mujeres = m[,c(rep(1:7)*2 + 1)]

# 1. hallamos las medias para cada edad (log)
alpha.h = rowMeans(log(m.hombres))
alpha.m = rowMeans(log(m.mujeres))

o = matrix(1, nrow = 18, ncol = 7)

# 2. centramos la matriz de mortalidad (log)
A.h = as.matrix(log(m.hombres)) - alpha.h*o
A.m = as.matrix(log(m.mujeres)) - alpha.m*o

# 3. valores singulares
svd.h = svd(A.h)
svd.m = svd(A.m)

# 4. estimación

#   hombres
beta.h = svd.h$u[,1]; 
kappa.h = svd.h$d[1]*svd.h$v[,1]; 

#   mujeres
beta.m = svd.m$u[,1]; 
kappa.m = svd.m$d[1]*svd.m$v[,1];


# 5. normalización 
#   centrado hombres

beta.h = beta.h/sum(beta.h); 
kappa.h = kappa.h*sum(beta.h) ; 
kappa.h = (kappa.h - mean(kappa.h))
#   centrado mujeres

beta.m = beta.m/sum(beta.m); 
kappa.m = kappa.m*sum(beta.m); 
kappa.m = (kappa.m - mean(kappa.m))

# 6. se modela kappa_t

par(mfrow = c(1,2))
plot(kappa.h) # comportamiento lineal
plot(kappa.m) # parece lineal también

m.arima.h = Arima(kappa.h, order = c(0,1,1)); summary(m.arima.h)
checkresiduals(m.arima.h) # no hay estacionariedad con (0,1,0)


m.arima.m = Arima(kappa.m, order = c(0,1,1)); summary(m.arima.m)
checkresiduals(m.arima.m)


par(mfrow = c(1,1))

fore.k.h = forecast(m.arima.h, h = 1)[4]$mean[1]
kappa.h
fore.k.h

fore.k.m = forecast(m.arima.m, h = 1)[4]$mean[1]
kappa.m
fore.k.m

# 7. proyección

kappa.h = c(kappa.h, fore.k.h) 
kappa.m = c(kappa.m, fore.k.m)

par(mfrow = c(1,2))
plot(kappa.h) # comportamiento lineal
plot(kappa.m) # parece lineal también
par(mfrow = c(1,1))

o = cbind(o, 1)

A.h.est.m = o*alpha.h + beta.h%*%t(kappa.h)

A.m.est.m = o*alpha.m + beta.m%*%t(kappa.m)

# pasamos a la tabla nueva

 # tasa mortandad en hombres
m.hombres = data.frame(m.hombres, H_2020_2025.proyección = exp(A.h.est.m)[,8])
rownames(m.hombres) = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                        "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                        "60 a 64","65 a 69","70 a 74","75 a 79","80 +")
m.hombres.log = m.hombres

 # tasa mortandad en mujeres
m.mujeres = data.frame(m.mujeres, M_2020_2025.proyección = exp(A.m.est.m)[,8])
rownames(m.mujeres) = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                        "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                        "60 a 64","65 a 69","70 a 74","75 a 79","80 +")
m.mujeres.log = m.hombres


###############################################
###### estimación por medio de q (logit) ######
###############################################

q = data.frame(
  Edad = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
           "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
           "60 a 64","65 a 69","70 a 74","75 a 79","80 +"),
  
  H_1985_1990 = c(0.047695,0.018481,0.003246,0.003342,0.008912,0.019125,
                  0.023035,0.024574,0.020720,0.023383,0.032933,0.044315,
                  0.073830,0.106825,0.150684,0.214995,0.310774,1.000000),
  M_1985_1990 = c(0.038542,0.016598,0.002372,0.001941,0.003751,0.004795,
                  0.005984,0.007508,0.009225,0.013688,0.020184,0.030780,
                  0.039895,0.087597,0.114331,0.170423,0.259978,1.000000),
  
  H_1990_1995 = c(0.036526,0.012654,0.002710,0.003245,0.016931,0.028696,
                  0.028496,0.026462,0.027918,0.027688,0.029955,0.040726,
                  0.061681,0.086366,0.137276,0.195663,0.285273,1.000000),
  M_1990_1995 = c(0.028241,0.011210,0.001943,0.001956,0.004161,0.004832,
                  0.004496,0.006533,0.008755,0.012040,0.017835,0.028160,
                  0.040777,0.068039,0.109603,0.167236,0.255012,1.000000),
  
  H_1995_2000 = c(0.032743,0.009231,0.002628,0.003190,0.014392,0.025269,
                  0.025148,0.023423,0.024462,0.024551,0.027196,0.037988,
                  0.057007,0.080231,0.125538,0.182069,0.274118,1.000000),
  M_1995_2000 = c(0.023884,0.007818,0.001777,0.001845,0.003633,0.004655,
                  0.004914,0.005871,0.007894,0.010766,0.016266,0.025104,
                  0.038903,0.059078,0.094865,0.147670,0.226251,1.000000),
  
  H_2000_2005 = c(0.028545,0.006220,0.002528,0.003123,0.011744,0.021551,
                  0.021507,0.020107,0.020734,0.021122,0.024098,0.034821,
                  0.048008,0.073163,0.112252,0.166379,0.260770,1.000000),
  M_2000_2005 = c(0.020124,0.005420,0.001622,0.001738,0.003165,0.004481,
                  0.004386,0.005266,0.007105,0.009608,0.014812,0.022337,
                  0.032753,0.051177,0.082222,0.130122,0.200332,1.000000),
  
  H_2005_2010 = c(0.023640,0.004880,0.002100,0.002530,0.009200,0.016660,
                  0.016650,0.015860,0.016830,0.018070,0.023240,0.034200,
                  0.046330,0.073980,0.111380,0.168250,0.244120,1.000000),
  M_2005_2010 = c(0.015920,0.004520,0.001490,0.001490,0.007270,0.008310,
                  0.003780,0.004020,0.005260,0.008580,0.013090,0.019830,
                  0.029810,0.046120,0.075680,0.121420,0.201100,1.000000),
  
  H_2010_2015 = c(0.020600,0.003690,0.001790,0.002010,0.006950,0.012340,
                  0.012360,0.012120,0.013380,0.015390,0.018900,0.027200,
                  0.044850,0.070990,0.112670,0.169890,0.263860,1.000000),
  M_2010_2015 = c(0.013470,0.003680,0.001190,0.001240,0.002270,0.003000,
                  0.002360,0.002510,0.005460,0.007600,0.010790,0.016720,
                  0.027900,0.042680,0.069810,0.131770,0.182160,1.000000),
  
  H_2015_2020 = c(0.018340,0.002860,0.001020,0.001640,0.005380,0.009020,
                  0.009300,0.009550,0.009370,0.013510,0.018690,0.026820,
                  0.043820,0.073980,0.117050,0.171050,0.264870,1.000000),
  M_2015_2020 = c(0.011730,0.002970,0.001020,0.001040,0.001910,0.002240,
                  0.002730,0.003000,0.004790,0.006790,0.016440,0.024720,
                  0.034620,0.069430,0.106340,0.174820,0.209000,1.000000)
)

logit = function(x){
  return(log(x/(1-x)))
}
sigmoid = function(x){
  return(1/(1+exp(-x)))
}

# la función logit tira para infinito cuando usamos q = 1:
q. = q[-18,]

q.hombres = q.[,c(rep(1:7)*2)]
q.mujeres = q.[,c(rep(1:7)*2 + 1)]

# 1. hallamos las medias para cada edad (log)
alpha.h = rowMeans(logit(q.hombres))
alpha.m = rowMeans(logit(q.mujeres))

o = matrix(1, nrow = 17, ncol = 7)

# 2. centramos la matriz de mortalidad (log)
A.h = as.matrix(logit(q.hombres)) - alpha.h*o
A.m = as.matrix(logit(q.mujeres)) - alpha.m*o

# 3. valores singulares
svd.h = svd(A.h)
svd.m = svd(A.m)

# 4. estimación
#   hombres
beta.h = svd.h$u[,1];
kappa.h = svd.h$d[1]*svd.h$v[,1]; 
#   mujeres
beta.m = svd.m$u[,1];
kappa.m = svd.m$d[1]*svd.m$v[,1];


# 5. normalización 
#   centrado hombres

beta.h = beta.h/sum(beta.h);
kappa.h = kappa.h*sum(beta.h) ;
kappa.h = -(kappa.h - mean(kappa.h))
#   centrado mujeres

beta.m = beta.m/sum(beta.m);
kappa.m = kappa.m*sum(beta.m);
kappa.m = -(kappa.m - mean(kappa.m))

# 6. se modela kappa_t

par(mfrow = c(1,2))
plot(kappa.h) # comportamiento lineal
plot(kappa.m) # parece lineal también


m.arima.h = Arima(kappa.h, order = c(1,1,1), include.drift = T, include.mean = T); summary(m.arima.h)
checkresiduals(m.arima.h) 

m.arima.m = Arima(kappa.m, order = c(1,1,1), include.mean = T, include.drift = F); summary(m.arima.m)
checkresiduals(m.arima.m)

par(mfrow = c(1,1))

fore.k.h = forecast(m.arima.h, h = 1)[4]$mean[1]
kappa.h
fore.k.h

fore.k.m = forecast(m.arima.m, h = 1)[4]$mean[1]
kappa.m
fore.k.m

# 7. proyección

kappa.h = c(kappa.h, fore.k.h) 
kappa.m = c(kappa.m, fore.k.m)

par(mfrow = c(1,2))
plot(kappa.h) # comportamiento lineal
plot(kappa.m) # parece lineal también
par(mfrow = c(1,1))

o = cbind(o, 1)

A.h.est.q = o*alpha.h + beta.h%*%t(kappa.h)

A.m.est.q = o*alpha.m + beta.m%*%t(kappa.m)

# pasamos a la tabla nueva

q.hombres = q[,c(rep(1:7)*2)]
q.mujeres = q[,c(rep(1:7)*2 + 1)]

sgmoid.h = rbind(sigmoid(A.h.est.q),1) # probabilidad de muerte en hombres

sgmoid.m = rbind(sigmoid(A.m.est.q),1) # probabilidad de muerte en mujeres

# tasa mortandad en hombres
q.hombres = data.frame(q.hombres, H_2020_2025.proyección = sgmoid.h[,8])
rownames(q.hombres) = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                        "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                        "60 a 64","65 a 69","70 a 74","75 a 79","80 +")
q.hombres.logit = q.hombres

# tasa mortandad en mujeres
q.mujeres = data.frame(q.mujeres, M_2020_2025.proyección = sgmoid.m[,8])
rownames(q.mujeres) = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                        "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                        "60 a 64","65 a 69","70 a 74","75 a 79","80 +")
q.mujeres.logit = q.mujeres


#############################################
###### estimación por medio de q (log) ######
#############################################


q.hombres = q[,c(rep(1:7)*2)]
q.mujeres = q[,c(rep(1:7)*2 + 1)]

# 1. hallamos las medias para cada edad (log)
alpha.h = rowMeans(log(q.hombres))
alpha.m = rowMeans(log(q.mujeres))

o = matrix(1, nrow = 18, ncol = 7)

# 2. centramos la matriz de mortalidad (log)
A.h = as.matrix(log(q.hombres)) - alpha.h*o
A.m = as.matrix(log(q.mujeres)) - alpha.m*o

# 3. valores singulares
svd.h = svd(A.h)
svd.m = svd(A.m)

# 4. estimación

#   hombres
beta.h = svd.h$u[,1];
kappa.h = svd.h$d[1]*svd.h$v[,1]; 

#   mujeres
beta.m = svd.m$u[,1];
kappa.m = svd.m$d[1]*svd.m$v[,1];

# 5. normalización 
#   centrado hombres

beta.h = beta.h/sum(beta.h);
kappa.h = kappa.h*sum(beta.h) ;
kappa.h = -(kappa.h - mean(kappa.h))
#   centrado mujeres

beta.m = beta.m/sum(beta.m);
kappa.m = kappa.m*sum(beta.m);
kappa.m = -(kappa.m - mean(kappa.m))

# 6. se modela kappa_t

par(mfrow = c(1,2))
plot(kappa.h)
plot(kappa.m) 

# ajuste de serie
m.arima.h = Arima(kappa.h, order = c(0,1,1), include.mean = F, include.drift = T); summary(m.arima.h)
checkresiduals(m.arima.h) 


m.arima.m = Arima(kappa.m, order = c(0,1,1), include.mean = F, include.drift = T); summary(m.arima.m)
checkresiduals(m.arima.m) 

par(mfrow = c(1,1))

fore.k.h = forecast(m.arima.h, h = 1)[4]$mean[1]
kappa.h
fore.k.h

fore.k.m = forecast(m.arima.m, h = 1)[4]$mean[1]
kappa.m
fore.k.m

# 7. proyección

kappa.h = c(kappa.h, fore.k.h) 
kappa.m = c(kappa.m, fore.k.m)

par(mfrow = c(1,2))
plot(kappa.h) # comportamiento lineal
plot(kappa.m) # parece lineal también
par(mfrow = c(1,1))

o = cbind(o, 1)

A.h.est.q = o*alpha.h + beta.h%*%t(kappa.h)

A.m.est.q = o*alpha.m + beta.m%*%t(kappa.m)

# nueva tabla
q.hombres = data.frame(q.hombres, H_2020_2025.proyección = exp(A.h.est.q)[,8])      # tasa mortandad en hombres
rownames(q.hombres) = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                        "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                        "60 a 64","65 a 69","70 a 74","75 a 79","80 +")
q.hombres.log = q.hombres

q.mujeres = data.frame(q.mujeres, M_2020_2025.proyección = exp(A.m.est.q)[,8])      # tasa mortandad en mujeres

rownames(q.mujeres) = c("0","1 a 4","5 a 9","10 a 14","15 a 19","20 a 24","25 a 29",
                        "30 a 34","35 a 39","40 a 44","45 a 49","50 a 54","55 a 59",
                        "60 a 64","65 a 69","70 a 74","75 a 79","80 +")

q.mujeres.log = q.mujeres

##################################
########### RESULTADOS ###########
##################################

# ESTIMACIÓN CON TASA DE MORTANDAD (LOG)

m.hombres.log
m.mujeres.log

# ESTIMACIÓN CON PROBABILIDAD DE MUERTE (LOGIT)

q.hombres.logit
q.mujeres.logit

# ESTIMACIÓN CON PROBABILIDAD DE MUERTE (LOG)

q.hombres.log
q.mujeres.log