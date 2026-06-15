############################################################
#### 1. CARGA INICIAL DE LOS ARCHIVOS
############################################################

### Primero limpiamos el entorno para evitar que objetos de análisis anteriores
### interfieran con esta actividad.
rm(list = ls())

### Cargamos el paquete tidyverse, que utilizaremos para manipular y organizar
### los datos durante el procesamiento.
install.packages("caret")
library(caret)
library(tidyverse)


### Indicamos la carpeta donde tenemos guardados los tres archivos.
### Debemos adaptar esta ruta a la ubicación correspondiente en nuestro ordenador.

 setwd("/Users/israelpenavargas/Desktop/Unir/Algoritmos_e_inteligencia_artificial/Actividades/Actividad_3__Taller_grupal__export/")

### Cargamos el archivo que contiene el ID y la clase de cada muestra.
### Como el archivo no tiene encabezado, utilizamos header = FALSE.
### También indicamos que las columnas están separadas por punto y coma.
clases_muestras <- read.csv(
  "classes.csv",
  header = FALSE,
  sep = ";",
  stringsAsFactors = FALSE
)

### Cargamos los nombres de los genes.
### Cada línea del archivo contiene el nombre de un gen.
nombres_genes <- readLines(
  "column_names.txt",
  warn = FALSE
)

### Cargamos la matriz de expresión génica.
### Las filas representan las muestras y las columnas representan los genes.
### Los valores están separados por punto y coma.
expresion_genica <- read.csv(
  "gene_expression.csv",
  header = FALSE,
  sep = ";",
  stringsAsFactors = FALSE
)

############################################################
#### 2. COMPROBACIÓN INICIAL DE LA ESTRUCTURA
############################################################

### Comprobamos las dimensiones del archivo de clases.
dim(clases_muestras)

### Comprobamos cuántos nombres de genes contiene el archivo.
length(nombres_genes)

### Comprobamos las dimensiones de la matriz de expresión génica.
dim(expresion_genica)


### Comprobamos que el número de muestras coincida entre el archivo de clases
### y la matriz de expresión génica.
coinciden_muestras <- nrow(clases_muestras) == nrow(expresion_genica)
coinciden_muestras

### Comprobamos que el número de nombres de genes coincida con el número
### de columnas de la matriz de expresión génica.
coinciden_genes <- length(nombres_genes) == ncol(expresion_genica)
coinciden_genes


### Los tres archivos presentan dimensiones compatibles.
### El archivo de clases contiene 801 muestras y la matriz de expresión génica
### también contiene 801 filas, por lo que cada fila puede asociarse con una muestra.

### Además, el archivo de nombres contiene 500 genes y la matriz de expresión
### tiene 500 columnas, por lo que podemos asignar correctamente un nombre
### de gen a cada columna.

### Las dos comprobaciones devolvieron TRUE, lo que confirma que los archivos
### pueden integrarse en un único dataframe sin pérdida de correspondencia.

############################################################
#### 3. REVISIÓN DEL CONTENIDO Y LOS TIPOS DE DATOS
############################################################

### Revisamos las primeras filas del archivo de clases para identificar
### qué información contiene cada columna.
head(clases_muestras)

### Revisamos la estructura del archivo de clases para conocer
### el tipo de dato almacenado en cada columna.
str(clases_muestras)

### Revisamos los primeros nombres de genes para comprobar
### que se han leído correctamente desde el archivo de texto.
head(nombres_genes, 10)

### Revisamos las primeras filas y columnas de la matriz de expresión génica.
### Mostramos solo una parte para evitar imprimir demasiados datos.
expresion_genica[1:5, 1:6]

### Revisamos la estructura general de la matriz de expresión génica.
str(expresion_genica)

### La revisión inicial muestra que el archivo de clases contiene dos columnas
### de tipo carácter. La primera columna corresponde al identificador de cada
### muestra y la segunda contiene la clase asignada a esa muestra.

### Los nombres de los genes se han leído correctamente desde el archivo de texto.

### La matriz de expresión génica contiene variables numéricas, por lo que puede
### utilizarse posteriormente en métodos de aprendizaje supervisado y no supervisado.

### En este momento, las columnas de la matriz todavía tienen nombres genéricos
### como V1, V2, V3, etc. En el siguiente paso sustituiremos esos nombres por
### los nombres reales de los genes.

############################################################
#### 4. ASIGNACIÓN DE NOMBRES A LAS COLUMNAS
############################################################

### Renombramos las dos columnas del archivo de clases.
### La primera contiene el identificador de cada muestra y la segunda
### contiene la clase biológica de la muestra.
colnames(clases_muestras) <- c("ID_muestra", "Clase")

### Asignamos a la matriz de expresión génica los nombres reales de los genes
### contenidos en el archivo column_names.txt.
colnames(expresion_genica) <- nombres_genes

### Comprobamos que los nuevos nombres se han asignado correctamente.
head(clases_muestras)

### Mostramos únicamente los primeros nombres de genes para mantener
### la salida limpia y fácil de interpretar.
head(colnames(expresion_genica), 10)

### Revisamos nuevamente una pequeña parte de la matriz para comprobar
### que las columnas ya aparecen con los nombres de los genes.
expresion_genica[1:5, 1:6]

### La asignación de nombres se realizó correctamente.
### El archivo de clases ahora contiene las columnas ID_muestra y Clase,
### mientras que las 500 columnas de expresión génica tienen los nombres
### reales de los genes.

### Esto facilita la interpretación de los datos y permitirá identificar
### claramente qué genes participan en los análisis posteriores.

############################################################
#### 5. CREACIÓN DEL DATAFRAME ÚNICO
############################################################

### Unimos el identificador y la clase de cada muestra con la matriz
### de expresión génica.
### Como los dos objetos tienen 801 filas y mantienen el mismo orden,
### cada fila de clases_muestras corresponde a la misma fila de
### expresion_genica.
datos_completos <- cbind(
  clases_muestras,
  expresion_genica
)

### Comprobamos las dimensiones del nuevo dataframe.
dim(datos_completos)

### Mostramos solo las primeras filas y algunas columnas
### para mantener la salida limpia.
datos_completos[1:5, 1:8]

### Revisamos la estructura general del dataframe integrado.
str(datos_completos)

### La integración de los datos se realizó correctamente.
### El dataframe final contiene 801 muestras y 502 columnas:
### una columna con el identificador de cada muestra, una columna con la clase
### y 500 columnas con los valores de expresión génica.

### La estructura también confirma que las variables de expresión son numéricas,
### mientras que ID_muestra y Clase contienen información descriptiva.
### Por tanto, ya disponemos del formato solicitado en la actividad para continuar
### con el procesamiento de los datos.

############################################################
#### 6. REVISIÓN DE VALORES PERDIDOS
############################################################

### Comprobamos el número total de valores perdidos en el dataframe completo.
total_na <- sum(is.na(datos_completos))
total_na

### Calculamos el porcentaje total de valores perdidos respecto al número
### total de celdas del dataframe.
porcentaje_na <- round(
  total_na / (nrow(datos_completos) * ncol(datos_completos)) * 100,
  4
)

porcentaje_na

### Comprobamos cuántos valores perdidos hay en cada columna.
na_por_columna <- colSums(is.na(datos_completos))

### Mostramos únicamente las columnas que contienen al menos un valor perdido.
columnas_con_na <- na_por_columna[na_por_columna > 0]
columnas_con_na

### Calculamos cuántas columnas presentan valores perdidos.
numero_columnas_con_na <- sum(na_por_columna > 0)
numero_columnas_con_na

### Comprobamos cuántas muestras contienen al menos un valor perdido.
numero_muestras_con_na <- sum(rowSums(is.na(datos_completos)) > 0)
numero_muestras_con_na

### No se detectaron valores perdidos en ninguna muestra.
### El número de muestras con al menos un valor NA fue igual a cero.

### Por tanto, no fue necesario aplicar ningún método de imputación,
### ya que el conjunto de datos está completo.

############################################################
#### 7. REVISIÓN DE DUPLICADOS
############################################################

### Comprobamos si existen identificadores de muestra duplicados.
### Cada muestra debería tener un ID único.
ids_duplicados <- sum(duplicated(datos_completos$ID_muestra))
ids_duplicados

### Comprobamos si existen filas completamente duplicadas.
### Esto permite detectar muestras repetidas con exactamente los mismos valores.
filas_duplicadas <- sum(duplicated(datos_completos))
filas_duplicadas

### Comprobamos si existen nombres de genes duplicados.
### Los nombres repetidos podrían generar confusión en los análisis posteriores.
genes_duplicados <- sum(duplicated(colnames(expresion_genica)))
genes_duplicados

### Mostramos los nombres de genes duplicados, en caso de que existan.
nombres_genes_duplicados <- colnames(expresion_genica)[
  duplicated(colnames(expresion_genica))
]

nombres_genes_duplicados


### No se detectaron identificadores de muestra duplicados, filas repetidas
### ni nombres de genes duplicados.

### Esto indica que cada muestra aparece una sola vez y que cada columna
### de expresión génica tiene un nombre único.

### Por tanto, no fue necesario eliminar registros ni renombrar genes antes
### de continuar con el procesamiento.

############################################################
#### 8. REVISIÓN DE GENES CON VARIANZA CERO O CASI CERO
############################################################


### Seleccionamos únicamente las columnas numéricas correspondientes
### a la expresión génica.
datos_genes <- datos_completos %>%
  dplyr::select(-ID_muestra, -Clase)

### Detectamos genes con varianza cero o casi cero.
genes_baja_varianza <- caret::nearZeroVar(
  datos_genes,
  saveMetrics = TRUE
)

### Comprobamos cuántos genes tienen varianza cero.
numero_genes_varianza_cero <- sum(genes_baja_varianza$zeroVar)
numero_genes_varianza_cero

### Comprobamos cuántos genes tienen varianza casi cero.
numero_genes_casi_cero <- sum(genes_baja_varianza$nzv)
numero_genes_casi_cero

### Mostramos los genes identificados como poco informativos.
genes_no_informativos <- rownames(genes_baja_varianza)[
  genes_baja_varianza$nzv
]

genes_no_informativos

### La revisión de la variabilidad detectó 3 genes con varianza cero
### y 9 genes con varianza casi cero.

### Los genes identificados como poco informativos fueron SHISAL2B, MIER3,
### ST3GAL6, ZCCHC12, ADGRA2, PARP14, RPL22L1, ZNF425 y RAB25.

### Estos genes presentan poca o ninguna variación entre las muestras, por lo que
### probablemente aportan escasa información para diferenciar las clases.

### Por este motivo, consideramos adecuado eliminarlos antes de aplicar los
### métodos de aprendizaje supervisado y no supervisado.

############################################################
#### 9. ELIMINACIÓN DE GENES CON VARIANZA CASI CERO
############################################################

### Eliminamos únicamente los genes identificados como poco informativos.
### Conservamos automáticamente el identificador, la clase y el resto
### de las variables de expresión génica.
datos_filtrados <- datos_completos %>%
  dplyr::select(-dplyr::all_of(genes_no_informativos))

### Comprobamos las dimensiones del dataframe después del filtrado.
dim(datos_filtrados)

### Comprobamos cuántos genes quedan disponibles para los análisis posteriores.
numero_genes_final <- ncol(datos_filtrados) - 2
numero_genes_final

### Revisamos las primeras filas y algunas columnas del dataframe filtrado.
datos_filtrados[1:5, 1:8]

### Después del filtrado, el dataframe quedó formado por 801 muestras
### y 493 columnas.

### De estas columnas, 491 corresponden a genes y las otras dos contienen
### el identificador y la clase de cada muestra.

### Por tanto, se eliminaron correctamente los 9 genes con varianza casi cero,
### manteniendo el resto de la información sin cambios.

############################################################
#### 10. REVISIÓN DE LA DISTRIBUCIÓN DE LAS CLASES
############################################################

### Contamos cuántas muestras pertenecen a cada clase.
frecuencia_clases <- table(datos_filtrados$Clase)
frecuencia_clases

### Calculamos el porcentaje que representa cada clase.
porcentaje_clases <- round(
  prop.table(frecuencia_clases) * 100,
  2
)

porcentaje_clases

### Comprobamos cuántas clases diferentes existen en el conjunto de datos.
numero_clases <- length(unique(datos_filtrados$Clase))
numero_clases

### El conjunto de datos contiene cinco clases diferentes: AGH, CFB, CGC,
### CHC y HPB.

### La distribución no es completamente equilibrada. La clase CFB es la más
### frecuente, con 300 muestras y un 37.45% del total, mientras que HPB es la
### menos representada, con 78 muestras y un 9.74%.

### Esta diferencia debe tenerse en cuenta en los modelos supervisados, porque
### el accuracy por sí solo podría favorecer a las clases mayoritarias.
### Por este motivo, será importante evaluar también sensibilidad, especificidad,
### precisión y F1 para cada clase.

############################################################
#### 11. ESCALADO DE LAS VARIABLES DE EXPRESIÓN GÉNICA
############################################################

### Seleccionamos únicamente las columnas de expresión génica.
### No incluimos ID_muestra ni Clase porque no son variables numéricas
### que deban ser escaladas.
matriz_genes_filtrada <- datos_filtrados %>%
  dplyr::select(-ID_muestra, -Clase)

### Centramos y escalamos los valores de expresión génica.
### El centrado hace que cada gen tenga media 0.
### El escalado hace que cada gen tenga desviación estándar 1.
matriz_genes_escalada <- scale(matriz_genes_filtrada)

### Convertimos la matriz escalada en dataframe.
datos_genes_escalados <- as.data.frame(matriz_genes_escalada)

### Volvemos a incorporar el identificador y la clase de cada muestra.
datos_escalados <- cbind(
  datos_filtrados[, c("ID_muestra", "Clase")],
  datos_genes_escalados
)

### Comprobamos las dimensiones del nuevo dataframe escalado.
dim(datos_escalados)

### Revisamos una pequeña parte del dataframe.
datos_escalados[1:5, 1:8]

### Comprobamos que los genes escalados tienen aproximadamente media 0.
round(colMeans(datos_genes_escalados[, 1:6]), 4)

### Comprobamos que los genes escalados tienen aproximadamente
### desviación estándar 1.
round(apply(datos_genes_escalados[, 1:6], 2, sd), 4)

### El escalado se realizó correctamente.
### El nuevo dataframe mantiene las 801 muestras y las 493 columnas,
### incluyendo el identificador, la clase y los 491 genes filtrados.

### Las medias de los genes escalados son aproximadamente 0 y sus
### desviaciones estándar son aproximadamente 1.

### Esto confirma que las variables se encuentran en una escala comparable,
### lo que resulta adecuado para métodos basados en distancias o varianza,
### como PCA, k-means, clustering jerárquico, kNN o SVM.


############################################################
#### 12. GUARDADO DE LOS DATOS PROCESADOS
############################################################

### Guardamos la versión filtrada sin escalado.
### Esta versión conserva los valores originales de expresión génica,
### pero ya no contiene los genes con varianza casi cero.
write.csv(
  datos_filtrados,
  "datos_filtrados.csv",
  row.names = FALSE
)

### Guardamos la versión filtrada y escalada.
### Esta versión será especialmente útil para métodos basados en distancias
### o en la varianza, como PCA, k-means, clustering jerárquico, kNN o SVM.
write.csv(
  datos_escalados,
  "datos_escalados.csv",
  row.names = FALSE
)

### Comprobamos que los archivos se han creado en la carpeta de trabajo.
file.exists("datos_filtrados.csv")
file.exists("datos_escalados.csv")

### Los dos archivos procesados se guardaron correctamente.
### Ambos contienen 801 muestras y 493 columnas, correspondientes al
### identificador de la muestra, la clase y 491 genes.

### La versión datos_filtrados.csv conserva los valores de expresión originales
### después de eliminar los genes con varianza casi cero.

### La versión datos_escalados.csv contiene esos mismos genes centrados y
### escalados, con media 0 y desviación estándar 1.

### Ninguno de los dos archivos contiene valores perdidos, por lo que están
### preparados para ser utilizados en los análisis supervisados y no supervisados.


#Borrar en el momento de Rmarkdown!
##datos_escalados.csv para PCA, k-means, clustering jerárquico, kNN o SVM.
##datos_filtrados.csv para árboles de decisión, Random Forest u otros métodos que no necesitan escalado.
