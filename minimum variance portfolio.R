
setwd("") # save stock data (250 days) as a xlsx archive.
datos = as.data.frame(readxl::read_xlsx("Libro1.xlsx", sheet = 1, col_names = T))

datos = apply(datos, 2, function(x) as.numeric(gsub(",", "", x)))


# clavamos logaritmo diferenciado



diff.log = function(datos){
  n = ncol(datos)
  datos = datos[nrow(datos):1, ]
  for (i in 1:n) {
    datos[,i] = log(datos[,i])
    datos[,i] = c(NA, diff(datos[,i]))
  }
  return(datos[-1,])
}

datos = diff.log(datos)

S = cov(datos)

V = matrix(c(rep(0,ncol(datos)),1), ncol = 1)
Mat = rbind(cbind(S, rep(-1)),(c(rep(1,ncol(S)),0)))
W = solve(Mat)%*%matrix(c(rep(0,ncol(datos)),1), ncol = 1)
W

# rentabilidades anuales
252*(W[-length(W)]*colMeans(datos))

# armaré el portafolio con Microsoft, Johnson y Nvidia

datos = datos[,c(1,8,9)]

S = cov(datos)

V = matrix(c(rep(0,ncol(datos)),1), ncol = 1)
Mat = rbind(cbind(S, rep(-1)),(c(rep(1,ncol(S)),0)))
W = solve(Mat)%*%matrix(c(rep(0,ncol(datos)),1), ncol = 1)
W
E = 252*(W[-length(W)]*colMeans(datos))*1000000

O = cbind(E,1)

M = t(O)%*%S%*%O

# defino 

R_f = 0.04
A = M[2]
B = M[1]
C = M[4]

ER_t = (B-A*R_f)/abs(A-C*R_f)
S2_t = (C*(R_f^2) - 2*A*R_f + B)/((A-(C*R_f))^2)

T_t = (ER_t - R_f)/sqrt(S2_t)

write.table(datos, file = "rendimientos MSFT - JNJ - NVDA.txt", row.names = F, sep = ";", col.names = T)
