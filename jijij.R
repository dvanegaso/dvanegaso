setwd("C:/Users/thetr/OneDrive/Documentos/R/bayesiana")
datos = read.csv("C:/Users/thetr/OneDrive/Documentos/R/bayesiana/SB11_20232_muestra.txt"
                 ,sep = ";"
                 , stringsAsFactors = FALSE)
table(datos$ESTU_DEPTO_RESIDE)
