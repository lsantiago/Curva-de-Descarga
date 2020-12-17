## Lectura de datos ----
# TODO: Agregar nuevos datos de prueba en carpeta data
dCaudalesSantaAna = read.csv("data/caudales.csv", sep = ";", header=T)

## Definición de función para cálculo de curva de descarga ----
curvaDescarga <- function(df, ho=0.25){
  ## ho: Altura inicial (cuando el caudal es 0)
  
  # Extracción de alturas
  h <- df[,2]

	# Extracción de caudales
  caudal <- df[,3]
  
  # Cálculo de variación (h-ho)
  variacion <- h - ho
  
  # Cáculo de Y
  Y <- log10(variacion)
  
  # Cálculo de X
  X <- log10(caudal)
  
  # Cálculo de n
  # Referencia: https://bit.ly/383UCF9
  #n <- c(summary(lm(X~Y))$coef[1,1], summary(lm(X~Y))$coef[2,1])
  mod <- lm(X~Y)
  n <- summary(mod)$coef[2,1]
  
  # Cálcular a
	# TODO: Definir que es "a"
  a <- 10^(1.742)
  
  # Cálcular Q (caudal calculado)
  Q <- a * (variacion)^n
  
	# Gráfica de curva de descarga
	# TODO: Revisar información a utilizar para graficas
	#plot(Q, h, main = "Curva de descarga")

  # Gráfica de ajuste lineal
  plot(Y, X, pch=19)
  abline(mod, col="red")
  
	print("Finalizado: Visualiza la gráfica en Rplots.pdf")

}

## Uso de función de curvatura de descarga
curvaDescarga(dCaudalesSantaAna)



