#leemos los datos de entrada
#colocamos header=F ya que no tiene cabecera
#colocamos row.names=NULL debido a que no se encuentran enumeradas 
datos = read.csv(file = "h.csv", header = F, row.names = NULL)

head(datos)

# Ejemplo de regla para asignar clases
definir_clase = function(numero){
  # Si quisiera 10 clusters entonces selecciono 9 cortes.
  # Recuerde que, en el caso de R, cada n�mero es relativo a un color.
  if(numero < 5.0)
    return(1)
  else if(numero < 6.0)
    return(2)
  else if(numero < 7.0)
    return(3)
  else if(numero < 8.0)
    return(4)
  else if(numero < 9.0)
    return(5)
  else if(numero < 10.0)
    return(6)
  else if(numero < 11.0)
    return(7)
  else if(numero < 12.0)
    return(8)
  else if(numero < 13.0)
    return(9)
  else
    return(10)
}

#declaramos el arreglo origen que proviene de la data original
clase_origen <- array(datos$V4)

#en esta variable guardaremos las clases para cada uno de los datos
clase_destino <- array(dim=1000)

#obtenemos el arreglo de clases
for (i in 1:1000) {
  clase_destino[i] <- definir_clase(clase_origen[i])
}

#si deseamos ver la clase destino imprimimos
#clase_destino

#asignamos los valores al data set original
datos$V4 = clase_destino

#si queremos validar el contenido de la data imprimimos
#datos$V4

# Copiamos el dataset en una variable nueva
entrada.num = datos

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada.num$V4 = NULL

# De DataFrame a Matrix
entrada.num = as.matrix(entrada.num)

# Matriz de distancia, ?dist para otras opciones distinta a norma 2
distancia = dist(entrada.num, diag = FALSE, upper = TRUE, p=6)

# M�todo por defecto es complete link
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
table(datos$V4)

#calculamos la matriz de confusi�n
corte = cutree(cluster, k=10)
table(datos$V4, corte)

#aplicamos cortes si deseamos ver menor cantidad de ramas
#en este caso se veran solo aquellas ramas superiores a 10
cortes = cut(dendrogram, h = 10)$upper
plot(cortes,
     xlab = "Ramas",
     ylab = "Alturas",
     main = "Dendograma")

