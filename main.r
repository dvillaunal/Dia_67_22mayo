## ---- eval=FALSE, include=TRUE--------------------------------------------------------------------------
## "Protocolo:
## 
## 1. Daniel Felipe Villa Regifo
## 
## 2. Lenguaje: R
## 
## 3. Tema: Cree funciones que manejen arreglos en R  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)
## 
## 4. Fuentes:
##    https://www.generatedata.com"


## -------------------------------------------------------------------------------------------------------
# Importemos la base:
base1 <- read.csv(file = "base1.csv", header = T, sep = ",", dec = ".")

## Creamos el array con las matrices:
a1 <- array(c(base1$Cantidad, base1$Lactosa, base1$Calorias, base1$Proteinas),
            dim = c(8,8,4))

# Ahora calculamos la función promedio basico de lactosa, con el promdeio de calorias y su producción más acercada a este:

produccion <- function(x){
  "Calcula promedio de lactosa y calorias"
  promlac <- function(){
    "Calcula el promedio de lactosa"
    l <- as.vector(x[,,2])
    proml <- round(mean(l), digits = 0)
    return(proml)
  }
  compromcal <- function(){
    "Compara el promedio basico de las calorias por litro"
    # Convertimos las cal a Kcal:
    Kcal <- x[,,3]/1000
    #le restamos los 200 kcal promedio:
    promKcal <- Kcal-100
    count <- 0
    meanKcal <- c()
    for (i in promKcal) {
      if(i >= 0){
        meanKcal <- c(meanKcal,i)
      }
    }
    for (i in promKcal) {
      count = count + 1
      if(i == min(meanKcal)){
        p <- paste("La Producción Numero: ", count, ", Tiene la tasa más cercana al promedio de calorias con una desviación del: ", i)
        return(p)
        break
      }
    }
  }
  data <- data.frame("Promedio de Lactosa de las 50 Producciones"= promlac(),
                     "Comparacion de Calorias con una bolsa estandar de leche"= compromcal())
  
  return(data)
}

# Miramos el resultado:
produccion(a1)

## Ahora exportamos el resultado:
write.table(produccion(a1), file = "LacKcal.txt", row.names = F, sep = " | ")


## -------------------------------------------------------------------------------------------------------
# Importamos la base:
base2 <- read.csv(file = "Base2.csv", sep = ",", header = T)

a2 <- array(c(base2$Impares, base2$Pares, base2$Primos), dim = c(10,10,3))

# Creamos la funcion para ordenar y multiplicar:
orden <- function(x){
  "Convierte matrices en vector y junta las primeras dos, con la tercera matriz se multiplica"
  v1 <- as.vector(x[,,1])
  v2 <- as.vector(x[,,2])
  
  v1 <- v1[order(x[,,1])]
  v2 <- v2[order(x[,,2])]
  
  v3 <- c(v1,v2)
  v3 <- v3[order(v3)]
  
  vp <- as.vector(x[,,3])
  vp <- vp[order(vp)]
  mp <- matrix(vp, nrow = 10)
  
  a3 <- array(v3, dim = c(10,10,2))
  a3[,,1] <- a3[,,1]*mp
  a3[,,2] <- a3[,,2]*mp
  return(a3)
}

orden(a2)

# Exportamos el resultado:
write.table(orden(a2), file = "NumParPrim.txt", row.names = F, sep = "||")
