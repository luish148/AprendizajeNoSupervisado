#leemos los datos de entrada
#colocamos header=F ya que no tiene cabecera
#colocamos row.names=NULL debido a que no se encuentran enumeradas 
datos = read.csv(file = "good_luck.csv", header = F, row.names = NULL)

# Copiamos el dataset en una variable nueva
entrada.num = datos

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada.num$V11 = NULL

# De DataFrame a Matrix
entrada.num = as.matrix(entrada.num)

# Matriz de distancia, ?dist para otras opciones distinta a norma 2
distancia = dist(entrada.num, diag = FALSE, upper = TRUE, p=6)
#dist()

# Método por defecto es complete link
metodo = "complete"
cluster = hclust(distancia, method = metodo)

#graficamos el cluster
plot(cluster)
#graficamos el cluster si lo queremos con nombres de coordenadas distintos
plot(cluster,
     xlab = "Ramas",
     ylab = "Alturas",
     main = "Dendograma")

# Verifiquemos el dendrograma
dendrogram = as.dendrogram(cluster)

#si queremos observar la cantidad de cada clase
table(datos$V11)

#calculamos la matriz de confusión
corte = cutree(cluster, k=2)
table(datos$V11, corte)

#aplicamos cortes si deseamos ver menor cantidad de ramas
#en este caso se veran solo aquellas ramas superiores a 6
cortes = cut(dendrogram, h = 6)$upper
plot(cortes,
     xlab = "Ramas",
     ylab = "Alturas",
     main = "Dendograma")

