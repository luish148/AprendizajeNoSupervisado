#leemos los datos de entrada
#colocamos header=F ya que no tiene cabecera
#colocamos row.names=NULL debido a que no se encuentran enumeradas 
datos = read.csv(file = "a.csv", header = F, row.names = NULL)

#para visualizar la data
#datos

#Podemos visualizar la cabecera de los datos
head(datos)

#Queremos conocer cuantos individuos de cada clase tenemos
#en este caso tenemos 1000 de cada clase
table(datos$V3)

#Para graficar los valores con los colores
plot(datos$V1,
     datos$V2,
     col = datos$V3,
     xlab = "X",
     ylab = "Y",
     main = "Clustering Rectangular")

#usando el modelo de k-medias
modelo.k.medias = kmeans(x = datos[, c("V1", "V2")],
                         centers = 3)

#datos originales con colores de cluster
plot(x = datos$V1,
     y = datos$V2,
     col = modelo.k.medias$cluster)

# Ahora graficamos los puntos
points(x = modelo.k.medias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)

#vemos el rendimientos del modelo con la matriz de confusión
table(modelo.k.medias$cluster, datos$V3)

