datos = read.csv("C:/Users/thetr/OneDrive/Documentos/R/bayesiana/SB11_20232_muestra.txt", sep = ";")

library(dplyr)
library(ggplot2)

# número de agrupamientos (deptos)
(m = length(table(datos$ESTU_DEPTO_RESIDE)))

n = dim(datos)[1]

# guardado de datos

# x: puntaje de los estudiantes en el país
# X: puntaje de los estudiantes en cada depto
# id: identificación de los departamentos
# n_j: número de estudiantes por departamento
# xd: promedio por cada departamento
# s2d: varianza por cada departamento

x = datos$PUNT_MATEMATICAS

X = vector(mode = "list",length = m) # guardamos un vector de listas donde cada 
# entrada son los puntajes del respectivo departamento

id = rep(NA,n) # declaramos de manera previa la ubicación de cada estudiante
m_d = rep(NA,m) # medias departamentales
s_d = rep(NA,m) # desviaciones estandar departamentales
m_m = rep(NA,p) 
s_m = rep(NA,p)
n_j = rep(NA,m) # tamaño de cada grupo


for (i in 1:m) {
  idx = datos$ESTU_COD_RESIDE_DEPTO == sort(unique(datos$ESTU_COD_RESIDE_DEPTO))[i]
  # lógico de pertenencia por cada estudiante a cada departamento
  id[idx] = i # se cambia la nomenclatura del dane con una que está entre 1 y 33
  # para luego asignar en id a cada estudiande a su correspondiente depto
  X[[i]] = x[idx]
  # se guarda la lista de puntajes de cada departamento que corresponde
  m_d[i] = mean(X[[i]])             # media departamental
  s_d[i] = sd(X[[i]])               # desviación destándar departamental
  n_j[i] = length(X[[i]])           # número de estudiantes por departamento
}

m_c = mean(datos$PUNT_MATEMATICAS)  # media global del país
s_c = sd(datos$PUNT_MATEMATICAS)    # desviación estándar del país

modelo = function(r = 10000, x, n_j, m_c, s_c, mu0, g20, eta0, t20, lam0, al0, be0, nus0)
{}
