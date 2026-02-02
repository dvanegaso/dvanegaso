#####################################
############## punto  1 #############
#####################################

# Al realizar la transfomación logit(\theta) a una distribución uniforme
# f(x) = \frac{1}{\theta}, bajo el teorema de la transformación, nos queda
# exactamente la logit inversa (distribución logística), 
# es decir f(x) = \frac{1}\frac{1+\exp{-\gamma}}. Esto implica que nos va a 
# quedar una distribución logística al aplicar montecarlo:


set.seed(21)

# simulamos una uniforme de parametros a = 0, b = 1:
MC = runif(n = 1000000,min = 0,max = 1)

# aplicamos la transfomación logit:
logit_MC = log(MC/(1-MC))

# comparamos ambas distribuciones
par(mfrow = c(1,2))
hist(MC,breaks = 100)
hist(logit_MC,breaks = 100,xlim = c(-10,10),ylim = c(0,0.3),freq = F)
par(mfrow = c(1,1))

#####################################
############## punto  2 #############
#####################################

# a)

y_A = c(12,9,12,14,13,13,15,8,15,6)
y_B = c(11,11,10,9,9,8,7,10,6,8,8,9,7)

# Probabilidades previas de la cepa A
a_A = 120; b_A = 10 

# Probabilidades previas de la cepa B

a_B = 12; b_B = 1

# gráfica de ambas previas

par(mfrow = c(1,2))

curve(dgamma(x,shape = a_A,rate = b_A),from = 0,30,n = 1000)
curve(dgamma(x,shape = a_B,rate = b_B),from = 0,30,n = 1000)

par(mfrow = c(1,1))

# hacemos uso de un modelo gamma-poisson donde la predictiva
# es una binomial negativa que sigue los siguientes parámetros:

# r = \alpha + n\bar{y}
# p = \frac{\beta+n}{\beta+n+1}
n_simul = 1000000
set.seed(123)
y_pred_A = rnbinom(n = n_simul
                   ,size = a_A + sum(y_A)
                   ,prob = (b_A + length(y_A) )/(b_A + length(y_A) + 1))

y_pred_B = rnbinom(n = n_simul
                   ,size = a_B + sum(y_B)
                   ,prob = (b_B + length(y_B) )/(b_B + length(y_B) + 1))

# mediante esta simulación de monte carlo, procedemos a contar,
# con el uso de probabilidad posterior de éxito/fracaso, la proporción
# de valores positivos respecto a los valores totales.

y_dif_AB = y_pred_A-y_pred_B

y_dif_AB = y_dif_AB[y_dif_AB > 0]

# nos queda que la probabilidad de tener P(\theta_{A} > \theta{B}) es igual a:

length(y_dif_AB)/n_simul

# b)

# para este punto, nos tocará considerar hacer un for >:(

m = c(1:50)
Sims1 = matrix(data = NA, ncol = 2 ,nrow = length(m))
set.seed(123)
for (i in 1: length(m)) {
  y_dif_AB = rnbinom(n = n_simul
                     ,size = a_A + sum(y_A)
                     ,prob = (b_A + length(y_A) )/(b_A + length(y_A) + 1)
                     )-rnbinom(
                     n = n_simul
                     ,size = a_B*i + sum(y_B)
                     ,prob = (b_B*i + length(y_B) )/(b_B*i + length(y_B) + 1))
  
  y_dif_AB = y_dif_AB[y_dif_AB > 0]
  
  # nos queda que la probabilidad de tener P(\theta_{A} > \theta{B}) es igual a:
  
  Sims1[i,] = c(i,length(y_dif_AB)/n_simul)
}

# c)

n_simul = 1000000
set.seed(123)
y_pred_A = rnbinom(n = n_simul
                   ,size = a_A + sum(y_A)
                   ,prob = (b_A + length(y_A) )/(b_A + length(y_A) + 1))

y_pred_B = rnbinom(n = n_simul
                   ,size = a_B + sum(y_B)
                   ,prob = (b_B + length(y_B) )/(b_B + length(y_B) + 1))

# mediante esta simulación de monte carlo, procedemos a contar,
# con el uso de probabilidad posterior de éxito/fracaso, la proporción
# de valores positivos respecto a los valores totales.

y_mean_dif_AB = mean(y_pred_A)-mean(y_pred_B)

y_mean_dif_AB = y_mean_dif_AB > 0

# nos queda que la probabilidad de tener P(\theta_{A} > \theta{B}) es igual a:

y_mean_dif_AB

# Es decir que con una sola simulación, la media es mayor. Revisamos con todo:


m = c(1:50)
Sims2 = matrix(data = NA, ncol = 2 ,nrow = length(m))
set.seed(123)
c = 0
for (i in 1: length(m)) {
  
  y_dif_AB = mean(rnbinom(
    n = n_simul, 
    size = a_A + sum(y_A), 
    prob = (b_A + length(y_A) )/(b_A + length(y_A) + 1))) - mean(rnbinom(
      n = n_simul, 
      size = a_B + sum(y_B), 
      prob = (b_B + length(y_B) )/(b_B + length(y_B) + 1)))
  
  # nos queda que la probabilidad de tener P(\theta_{A} > \theta{B}) es igual a:
  y_dif_AB = y_dif_AB > 0
  
  c = c + y_dif_AB
}
c/length(m)



