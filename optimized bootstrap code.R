# Cargar los paquetes necesarios
library(foreach)
library(doParallel)

# Configurar el entorno de trabajo y cargar los datos
setwd("C:/Users/thetr/OneDrive/Escritorio/Parcial Inferencia III")
creditos <- read.delim("creditos.txt")

# Filtrar y renombrar variables
creditos <- creditos[, c("INGRESOS_DECLARADOS_TOTA", "SEXO", "MONTO_TOTAL_OTORGADO", "NIVEL_ESTUDIOS", "ESTRATOS")]
colnames(creditos) <- c("ingresos", "genero", "monto", "estudios", "estrato")

# Redefinir escala de los ingresos para trabajar en millones
creditos$ingresos <- creditos$ingresos / 1000000
creditos$monto <- creditos$monto / 1000000

# Filtrar datos por monto y género
creditos <- creditos[(creditos$monto < 100) & (creditos$ingresos < 25),]
creditos_hom <- creditos[creditos$genero == "H", ]
creditos_muj <- creditos[creditos$genero == "M", ]

# Extraer ingresos para hombres y mujeres
ingr_hom <- creditos_hom$ingresos
ingr_muj <- creditos_muj$ingresos

# 1. Definir la función de bootstrap
bootstrap <- function(data, n) {
  set.seed(2103)
  replicate(n, mean(sample(data, replace = TRUE)))
}

# 2. Detectar el número de núcleos disponibles
n_cores <- detectCores() - 1  # Usamos un núcleo menos para no sobrecargar el sistema

# 3. Crear el backend de paralelización
cl <- makeCluster(n_cores)  # Iniciar el cluster con n núcleos
registerDoParallel(cl)      # Registrar el backend de paralelización

# 4. Número de iteraciones de bootstrap
n_iterations <- 10001

# 5. Paralelizar el bootstrap para los hombres
resultados_hom <- foreach(i = 1:n_cores, .combine = 'c', .packages = 'base') %dopar% {
  bootstrap(ingr_hom, n_iterations / n_cores)
}

# 6. Detener el cluster para liberar recursos
stopCluster(cl)

# 7. Crear un nuevo cluster para mujeres
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# 8. Paralelizar el bootstrap para las mujeres
resultados_muj <- foreach(i = 1:n_cores, .combine = 'c', .packages = 'base') %dopar% {
  bootstrap(ingr_muj, n_iterations / n_cores)
}

# 9. Detener el cluster para liberar recursos
stopCluster(cl)

# 10. Asegurar que los resultados tengan la misma longitud antes de restar
min_length <- min(length(resultados_hom), length(resultados_muj))
resultados_hom <- resultados_hom[1:min_length]
resultados_muj <- resultados_muj[1:min_length]

# 11. Restar los resultados del bootstrap
resultados <- resultados_hom - resultados_muj

# Mostrar el resumen de los resultados

IC_095_dif_hom_muj_quartile_bootstrap<-quantile(resultados, probs = c(0.025,0.975))
IC_095_dif_hom_muj_quartile_bootstrap