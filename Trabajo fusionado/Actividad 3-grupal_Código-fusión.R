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
setwd("~/Documents/2026_MUBI/1_Algoritmos_AI/Actividades/Actividad_3-Taller_grupal/Actividad_3__Taller_grupal__data")


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




############################################################
#### 13. PREPARACIÓN DEL ENTORNO PARA APRENDIZAJE NO SUPERVISADO
############################################################

### factoextra permite visualizar de forma sencilla los resultados del PCA
### y de los métodos de clustering (scree plot, proyecciones, clusters...).
library(factoextra)

### cluster proporciona herramientas adicionales, como el cálculo del
### coeficiente de silueta, que utilizaremos para evaluar la calidad
### interna de los clusters obtenidos.
library(cluster)

### Rtsne implementa el algoritmo t-SNE, que utilizaremos como segunda
### técnica de reducción de dimensionalidad.

install.packages("Rtsne")
library(Rtsne)

### mclust nos permite calcular el índice de Rand ajustado (ARI), que
### utilizaremos para comparar de forma cuantitativa los clusters obtenidos
### con las clases biológicas reales.
library(mclust)

### 
library(dplyr)

### 
install.packages("plotly")
library(plotly)


############################################################
#### 14. COMPROBAMOS CARGA DE LOS DATOS PROCESADOS
############################################################


if (!exists("datos_escalados")) {
  datos_escalados <- read.csv(
    "datos_escalados.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )
}

### Comprobamos que el dataframe recuperado mantiene las dimensiones
### esperadas (801 muestras y 493 columnas).
dim(datos_escalados)

### Como se justificó en el apartado 11, utilizamos la versión escalada de
### los datos para los cuatro métodos no supervisados, ya que PCA, t-SNE,
### k-means y el clustering jerárquico se basan en varianzas o en
### distancias entre muestras, y por tanto requieren variables en una
### escala comparable.


############################################################
#### 15. PREPARACIÓN DE LA MATRIZ DE EXPRESIÓN PARA LOS MÉTODOS NO SUPERVISADOS
############################################################

### Separamos la matriz numérica de expresión génica de las columnas
### descriptivas (ID_muestra y Clase). Los algoritmos no supervisados no
### aceptan la clase como entrada: la clase solo se utilizará después,
### para interpretar y visualizar los resultados.
genes_no_supervisado <- datos_escalados %>%
  dplyr::select(-ID_muestra, -Clase)

### Asignamos el identificador de cada muestra como nombre de fila, lo que
### facilita la interpretación de los resultados en gráficos posteriores.
rownames(genes_no_supervisado) <- datos_escalados$ID_muestra

### Guardamos la clase real de cada muestra en un vector independiente.
### Se utilizará exclusivamente para colorear gráficos y comparar
### visualmente los grupos obtenidos con la clasificación biológica real,
### nunca como variable de entrada en los algoritmos no supervisados.
clase_real <- factor(datos_escalados$Clase)

### Comprobamos las dimensiones de la matriz que utilizaremos en los cuatro
### métodos no supervisados.
dim(genes_no_supervisado)


############################################################
#### 16. REDUCCIÓN DE DIMENSIONALIDAD (I): ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
############################################################

### El PCA transforma las 491 variables originales en un nuevo conjunto de
### variables no correlacionadas (componentes principales o PC), ordenadas según
### la cantidad de varianza que explican.

### Como los datos ya están centrados y escalados (apartado 11), indicamos
### center = FALSE y scale. = FALSE para no aplicar un segundo escalado.
pca_resultado <- prcomp(
  genes_no_supervisado,
  center = FALSE,
  scale. = FALSE
)

### Resumimos la varianza explicada por cada componente principal.
resumen_pca <- summary(pca_resultado)
pca.df <- as.data.frame(pca_resultado$x[, 1:3])
colnames(pca.df) <- c("X1", "X2", "X3")

### Mostramos la varianza explicada y la varianza acumulada de los primeros
### 10 componentes.
resumen_pca$importance[, 1:10]

### Calculamos manualmente la proporción de varianza explicada por cada
### componente a partir de su desviación estándar.
varianza_explicada <- (pca_resultado$sdev^2) / sum(pca_resultado$sdev^2)

### Calculamos la varianza acumulada, para identificar cuántos componentes
### son necesarios para retener un porcentaje determinado de la información
### original.
varianza_acumulada <- cumsum(varianza_explicada)

### Calculamos cuántos componentes son necesarios para explicar al menos
### el 80% de la varianza total. Este valor se reutilizará en el apartado
### siguiente como entrada para t-SNE.
n_componentes_80 <- which(varianza_acumulada >= 0.80)[1]
n_componentes_80

### Representamos un gráfico de sedimentación (scree plot) con la varianza
### explicada por los primeros 15 componentes.
fviz_eig(
  pca_resultado,
  addlabels = TRUE,
  ncp = 15,
  main = "Varianza explicada por componente (PCA)"
)

### Representamos las dos primeras componentes principales, coloreando cada
### muestra según su clase biológica real. Esto permite valorar visualmente
### si el PCA separa las clases utilizando solo la expresión génica, sin
### haber utilizado la clase durante el cálculo de las componentes.
fviz_pca_ind(
  pca_resultado,
  habillage = clase_real,
  addEllipses = TRUE,
  label = "none",
  geom = "point",
  pointsize = 1.5,
  title = "PCA - Muestras coloreadas por clase biológica"
)

### Visualizamos gráfico en 3D para obsevar como se separan los genes en el
### espacio según los primeros 3 componentes principales.
plot_ly(
  data = pca.df,
  x = ~X1,
  y = ~X2,
  z = ~X3,
  color = ~clase_real,
  colors = c("red", "blue", "green", "orange", "purple"),
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 5,
    opacity = 0.6
  )
) %>%
  plotly::layout(title = "PCA 3D - Muestras coloreadas por clase biológica")

### La primera componente principal (PC1) explica un 12.53% de la varianza
### total, y la segunda (PC2) un 9.52%, acumulando entre ambas solo un
### 22.05%. Esta leve progresión indica que la variabilidad de la expresión
### génica no está concentrada en unos pocos genes o patrones dominantes,
### sino que se encuentra distribuida entre un número elevado de componentes:
### fueron necesarios 81 componentes (n_componentes_80) para alcanzar el 80%
### de la varianza total, sobre un máximo posible de 491.

### En la proyección de PC1 frente a PC2 coloreada por clase biológica se
### observa una separación parcial: la clase AGH se muestra separada del
### resto, de manera relativamente diferenciada, mientras que las otras clases
### se solapan de forma considerablemente. Esto sugiere que, aunque los dos
### primeros componentes capturan parte de la señal biológica relevante,
### dos dimensiones no son suficientes para discriminar completamente las
### cinco clases, lo que es coherente con la baja varianza acumulada. 

### Para comprobar el efecto de añadir el tercer componente principal, realizamos
### una proyección en 3D. Observamos que la clase AGH se encuentra separada
### en los componentes principales 1 y 2 como ya se había visto en la 
### representación en 2D. En cambio, en esta representación sí que se observa 
### una separación del resto de clases en el componente principal 3. No
### obstante, la clase CFB muestra una mayor dispersión, observándose puntos
### entremezclados en las clases CGC y HPB. 

### Estos resultados motivan el uso de t-SNE como técnica complementaria 
### no lineal.


############################################################
#### 17. REDUCCIÓN DE DIMENSIONALIDAD (II): t-SNE
############################################################

### t-SNE es una técnica de reducción de dimensionalidad no lineal,
### que, a diferencia del PCA, no busca conservar la varianza global, sino
### las relaciones de vecindad locales: muestras similares en el espacio
### original deben permanecer próximas en el espacio reducido.

### Aplicar t-SNE directamente sobre las 491 variables originales puede ser
### lento y sensible al ruido. Por ello, siguiendo una práctica habitual,
### utilizamos como entrada las componentes principales que retienen el
### 80% de la varianza (calculadas en el apartado anterior) en lugar de la
### matriz completa de expresión génica.
entrada_tsne <- pca_resultado$x[, 1:n_componentes_80]

### Fijamos una semilla para garantizar la reproducibilidad, ya que t-SNE
### incluye un componente aleatorio en su inicialización.
set.seed(1993)

### Ejecutamos el algoritmo t-SNE. Mantenemos perplexity = 30 (valor
### adecuado para 801 muestras), desactivamos el PCA interno (pca = FALSE)
### porque ya hemos reducido la dimensionalidad en el apartado anterior, y
### desactivamos la comprobación de duplicados para evitar errores si
### existieran muestras con perfiles de expresión muy similares.
tsne_resultado <- Rtsne(
  entrada_tsne,
  dims = 2,
  perplexity = 30,
  pca = FALSE,
  verbose = TRUE,
  max_iter = 1000,
  check_duplicates = FALSE
)

### Creamos un dataframe con las dos dimensiones obtenidas y la clase real
### de cada muestra, para representarlas con ggplot2.
tsne_df <- data.frame(
  tSNE1 = tsne_resultado$Y[, 1],
  tSNE2 = tsne_resultado$Y[, 2],
  Clase = clase_real
)

### Representamos el resultado de t-SNE coloreando cada muestra según su
### clase biológica real.
ggplot(tsne_df, aes(x = tSNE1, y = tSNE2, color = Clase)) +
  geom_point(size = 1.5, alpha = 0.7) +
  labs(
    title = "t-SNE - Muestras coloreadas por clase biológica",
    x = "Dimensión t-SNE 1",
    y = "Dimensión t-SNE 2"
  ) +
  theme_minimal()

### A diferencia del PCA, donde CFB solapaba de forma considerable con CGC y HPB
### , t-SNE logra una separación mucho más clara: se observan
### cinco agrupaciones bien diferenciadas que se corresponden, en gran
### medida, con las cinco clases biológicas reales. Esto confirma que la
### relación entre la expresión génica y la clase biológica no es
### puramente lineal: una técnica no lineal, capaz de preservar las
### relaciones de vecindad local, separa los grupos con mayor claridad
### que una proyección lineal basada en varianza global como el PCA.

### No obstante, la separación no es perfecta. Se observa un punto de la
### clase HPB que se mezclan con la clase CFB, y puntos de la clase CGC
### que se solapan tanto con CFB como con HPB, como ya habíamos visto en el PCA 
### en 3D. Estas tres clases (CFB, CGC y HPB) parecen compartir, en algunas 
### muestras, perfiles de expresión génica más similares entre sí que con las 
### clases AGH y CHC, lo que podría dificultar su discriminación en los modelos 
### supervisados posteriores. Conviene recordar, además, que en t-SNE la 
### distancia entre agrupaciones no debe interpretarse como una magnitud
### cuantitativa, y que el resultado puede variar ligeramente entre
### ejecuciones con semillas distintas.


############################################################
#### 18. CLUSTERIZACIÓN (I): K-MEANS
############################################################

### k-means agrupa las muestras en k clusters, minimizando la distancia de
### cada muestra al centroide de su cluster. A diferencia del PCA y de
### t-SNE, no reduce el número de variables: asigna directamente cada
### muestra a un grupo.

### Antes de fijar el número de clusters, exploramos dos criterios
### habituales para seleccionar un valor adecuado de k.

### Método del codo (elbow method): representa la suma de cuadrados dentro
### de cada cluster (WSS) para distintos valores de k.
set.seed(1993)
fviz_nbclust(
  genes_no_supervisado,
  kmeans,
  method = "wss",
  k.max = 10
) +
  labs(title = "Método del codo para seleccionar k")

### Método de la silueta: representa el coeficiente de silueta medio para
### distintos valores de k. Valores más altos indican clusters mejor
### definidos.
set.seed(1993)
fviz_nbclust(
  genes_no_supervisado,
  kmeans,
  method = "silhouette",
  k.max = 10
) +
  labs(title = "Método de la silueta para seleccionar k")

### El método del codo muestra que la curva de WSS comienza a aplanarse en
### k = 5, lo que sugiere que, a partir de ese punto, añadir más clusters
### aporta una reducción cada vez menor de la varianza. El método de la
### silueta, por su parte, señala k = 6 como valor óptimo.

### Ambos criterios apuntan a un número de clusters próximo al número real
### de clases biológicas (cinco), aunque no coinciden exactamente entre
### sí: el codo en k = 5 es coherente con las clases reales, mientras que
### la silueta sugiere un cluster adicional. Esta pequeña discrepancia es
### razonable teniendo en cuenta lo observado en el apartado de PCA y t-SNE, 
### donde las clases CFB, CGC y HPB mostraban cierto solapamiento entre ellas: 
### es posible que, desde un punto de vista puramente basado en distancias,
### exista un subgrupo dentro de alguna de estas clases que el algoritmo
### detecte como un cluster diferenciado adicional.

### Dado que disponemos de información biológica previa sobre el número de
### clases (cinco), ejecutamos k-means con k = 5, lo que permite comparar
### directamente los clusters obtenidos con las clases reales.
set.seed(1993)
kmeans_resultado <- kmeans(
  genes_no_supervisado,
  centers = 5,
  nstart = 25
)

### Calculamos el coeficiente de silueta medio para evaluar la calidad
### interna de los clusters obtenidos.
distancias <- dist(genes_no_supervisado)
silueta_kmeans <- silhouette(kmeans_resultado$cluster, distancias)
mean(silueta_kmeans[, 3])

### Comparamos los clusters obtenidos con la clase biológica real mediante
### una tabla de contingencia. Esta tabla no debe interpretarse como una
### matriz de confusión propiamente dicha, ya que k-means no conoce las
### clases ni asigna directamente una etiqueta biológica a cada cluster.
table(
  Cluster = kmeans_resultado$cluster,
  Clase_real = clase_real
)

### Visualizamos los clusters obtenidos, proyectados sobre las dos primeras
### componentes principales.
fviz_cluster(
  kmeans_resultado,
  data = genes_no_supervisado,
  geom = "point",
  ellipse.type = "norm",
  pointsize = 1.5,
  main = "Clusters obtenidos mediante k-means (k = 5)"
)

### El coeficiente de silueta medio obtenido es 0.138 para k=5, un valor bajo que
### indica que, en conjunto, los clusters no están especialmente bien
### separados ni son muy compactos en el espacio de 491 genes. De manera similar, 
### para k=6 el coeficiente obtenido es 0.140, lo que indica que incluso en 6
### clusters la separación sigue siendo poco eficiente.

### En la tabla de contingencia para k=5 se muestra un panorama más matizado.
### Tres de los cinco clusters son prácticamente puros y se corresponden
### casi en exclusiva con una única clase biológica: el cluster 2 con CHC
### (133 de 136 muestras), el cluster 3 con AGH (142 de 146) y el cluster 5
### con HPB (75 de 78). El cluster 4 está dominado por la clase CFB (225 de
### sus 230 muestras), aunque incluye también algunas muestras de otras
### clases. Para k=6 observamos dos clusters puros (1 y 5), mientras que el resto
### presentan solapamientos entre clases, por lo que tampoco es suficiente para 
### separar correctamente a las clases.

### En la visualización para k=5, el cluster 1 es el que concentra la falta de separación: agrupa casi
### toda la clase CGC (139 de 141 muestras) junto con una parte importante
### de la clase CFB (75 de sus 300 muestras, un 25%). Esto indica que,
### dentro del espacio de expresión génica, un subconjunto de muestras CFB
### presenta un perfil más parecido al de CGC que al resto de su propia
### clase, lo que coincide con el solapamiento entre CFB y CGC observado
### previamente en la proyección de t-SNE y PCA, y explica en buena medida el bajo
### valor del coeficiente de silueta medio. En la visualización para k=6, el cluster 1 se separa
### muy bien del resto, pero los clusters 2-6 siguen solapándose.


############################################################
#### 19. CLUSTERIZACIÓN (II): CLUSTERING JERÁRQUICO
############################################################

### El clustering jerárquico no requiere fijar de antemano el número de
### clusters. Construye un árbol (dendrograma) que agrupa progresivamente
### las muestras más similares, permitiendo decidir el número de clusters
### a posteriori, cortando el árbol a la altura deseada.

### Calculamos la matriz de distancias euclídeas entre las muestras a
### partir de los valores de expresión génica escalados.
distancias_genes <- dist(genes_no_supervisado, method = "euclidean")

### Aplicamos el método de Ward, que tiende a generar clusters compactos y
### de tamaño similar, minimizando el incremento de varianza dentro de
### cada cluster en cada paso de la agrupación.
cluster_jerarquico <- hclust(distancias_genes, method = "ward.D2")

### Representamos el dendrograma resultante. Ocultamos las etiquetas
### individuales de las 801 muestras para mantener la legibilidad del
### gráfico.
plot(
  cluster_jerarquico,
  labels = FALSE,
  hang = -1,
  main = "Dendrograma - Clustering jerárquico (método de Ward)",
  xlab = "Muestras",
  ylab = "Distancia"
)

### Señalamos en el dendrograma los cinco grupos obtenidos al cortar el
### árbol, para poder comparar visualmente esta partición con las clases
### biológicas reales.
rect.hclust(cluster_jerarquico, k = 5, border = 2:6)

### Cortamos el árbol para obtener cinco clusters, el mismo número que el
### de clases biológicas presentes en los datos.
clusters_jerarquico <- cutree(cluster_jerarquico, k = 5)

### Calculamos el coeficiente de silueta medio para los clusters obtenidos
### mediante el método jerárquico.
silueta_jerarquico <- silhouette(clusters_jerarquico, distancias_genes)
mean(silueta_jerarquico[, 3])

### Comparamos los clusters obtenidos con la clase biológica real mediante
### una tabla de contingencia.
table(
  Cluster = clusters_jerarquico,
  Clase_real = clase_real
)

### El coeficiente de silueta medio obtenido (0.138) es prácticamente
### idéntico al obtenido con k-means k = 5 (0.1379), lo que indica una calidad
### interna de los clusters muy similar entre ambos métodos.

### La tabla de contingencia muestra un patrón parecido al de k-means k = 5: tres
### clusters son casi puros y se corresponden con una única clase
### biológica: el cluster 4 con AGH (145 de 146 muestras), el cluster 5 con
### HPB (77 de 78) y el cluster 1, mayoritariamente, con CHC (135 de 136),
### aunque este último incluye también 15 muestras de CFB. El cluster 2
### está dominado por CGC (140 de 141 muestras), pero incorpora 45 muestras
### de CFB. El cluster 3 agrupa la mayor parte de la clase CFB (240 de sus
### 300 muestras, un 80%).

### Por tanto, igual que con k-means, las clases AGH y HPB quedan
### claramente aisladas en clusters propios, mientras que la clase CFB es
### la que presenta mayor heterogeneidad: una parte importante de sus
### muestras se reparte entre los clusters dominados por CGC y por CHC, en
### lugar de formar un grupo homogéneo propio. A diferencia de k-means, que
### concentraba casi todo el solapamiento en un único cluster compartido
### con CGC, el método jerárquico reparte las muestras "ambiguas" de CFB
### entre dos clusters distintos (el de CGC y el de CHC), lo que sugiere
### que estas muestras de CFB no forman un subgrupo homogéneo, sino que se
### sitúan en una zona de transición entre varias clases.

### En los 4 métodos de aprendizaje no supervisado, el principal origen de desacuerdo con las clases
### reales es la clase CFB: una parte de sus muestras se confunde con CGC
### (en los dos métodos) y, en el caso del clustering jerárquico, también
### con CHC. Esto es coherente con lo observado tanto en la proyección de
### t-SNE como en las tablas de contingencia de los apartados anteriores, y
### sugiere que, desde el punto de vista de la expresión génica, un
### subconjunto de las muestras CFB comparte características con otras
### clases en mayor medida que con el resto de su propia clase. Esta
### observación será relevante a la hora de interpretar los errores de
### clasificación de los modelos supervisados que se implementarán a
### continuación.

############################################################
#### 20. IMPLEMENTACIÓN DE MÉTODOS DE APRENDIZAJE SUPERVISADO
############################################################

# Hemos elegido tres métodos de aprendizaje supervisados: SVM, Árbol de decisión 
# y Naive bayes. 

# Primero instalamos los paquetes necesarios:

install.packages("caret")
install.packages("e1071")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("dplyr")
install.packages("ggplot2")

library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(dplyr)

# Según la necesidad dictada por el tipo de método, emplearemos tanto los datos escalados 
# -para el modelo SVM y para Naïve Bayes- como los datos filtrados crudos para el 
# árbol de decisión, ya que éste no necesita escalado.  


# Visualizamos los datos que tenemos
names(datos_escalados)
dim(datos_escalados)
head(datos_escalados)

names(datos_filtrados)
dim(datos_filtrados)
head(datos_filtrados)

# Preparamos los datos escalados y filtrados, ya que la variable clase debe de
# ser un factor porque es la variable que queremos predecir.

datos_escalados$Clase <- as.factor(datos_escalados$Clase)
datos_filtrados$Clase <- as.factor(datos_filtrados$Clase)

# Eliminamos el identificador de muestra porque no aporta información biológica
# para clasificar.

datos_escalados <- datos_escalados %>%
  select(-ID_muestra)

datos_filtrados <- datos_filtrados %>%
  select(-ID_muestra)

# Fijamos la semilla para que los resultados sean reproducibles

set.seed(1995)

# Dividimos los datos en entrenamiento y test. Esto divide las 801 muestras en
# 80% entrenamiento y 20% test. Sirve para entrenar con unas muestras y comprobar
# si clasifica bien las que nunca ha visto.

trainIndex_escalados <- createDataPartition(
  datos_escalados$Clase,
  p = 0.8,
  list = FALSE
)

train_escalado <- datos_escalados[trainIndex_escalados, ]
test_escalado <- datos_escalados[-trainIndex_escalados, ]



trainIndex_filtrados <- createDataPartition(
  datos_filtrados$Clase,
  p = 0.8,
  list = FALSE
)

train_filtrado <- datos_filtrados[trainIndex_filtrados, ]
test_filtrado <- datos_filtrados[-trainIndex_filtrados, ]

# Aplicamos métodos de aprendizaje supervisados.

############################################################
#### 20.1 SVM
############################################################

# Entrenamos el modelo. Para ello indicamos que evalúe la variable genes, 
# que utilice únicamente las muestras de entrenamiento escaladas, elegimos el
# algoritmo, y activamos la validación cruzada con 5 entrenamientos para comprobar
# que el modelo es estable.

modelo_svm <- train(
  Clase ~ .,
  data = train_escalado,
  method = "svmRadial",
  trControl = trainControl(
    method = "cv",
    number = 5
  )
)

# Una vez entrenado el modelo, clasificamos las muestras nuevas, que son el 20&
# que habíamos guardado antes como test. En este caso también en datos escalados.

predicion_svm <- predict(
  modelo_svm,
  newdata = test_escalado
)
predicion_svm

# Analizamos las métricas por clases

metricasclases_svm <- confusionMatrix(
  predicion_svm,
  test_escalado$Clase
)

metricasclases_svm

# Obtenemos la matriz de confusión

table(
  Clase_predicha = predicion_svm,
  Clase_real = test_escalado$Clase
)

############################################################
#### 20.2 ÁRBOL DE DECISIÓN
############################################################

# Entrenamos al modelo, en este caso utilizamos los datos filtrados

modelo_arbol <- rpart(
  Clase ~ .,
  data = train_filtrado,
  method = "class"
)

# Evaluamos el modelo con el 20% de los datos restantes pertenecientes a test

predicion_arbol <- predict(
  modelo_arbol,
  newdata = test_filtrado,
  type = "class"
)

# Métricas por clases
metricasclases_arbol <- confusionMatrix(
  predicion_arbol,
  test_filtrado$Clase
)

metricasclases_arbol

# Obtenemos la matriz de confusión

table(
  Clase_predicha = predicion_arbol,
  Clase_real = test_filtrado$Clase
)

# Dibujamos árbol de decisión
rpart.plot(modelo_arbol)

############################################################
#### 20.3 NAIVE BAYES
############################################################

# Repetimos los mismos pasos que en los modelos anteriores

modelo_nb <- naiveBayes(
  Clase ~ .,
  data = train_escalado
)

predicion_nb <- predict(
  modelo_nb,
  newdata = test_escalado
)

metricasclases_nb <- confusionMatrix(
  predicion_nb,
  test_escalado$Clase
)

metricasclases_nb

# Obtenemos la matriz de confusión

table(
  Clase_predicha = predicion_nb,
  Clase_real = test_escalado$Clase
)

##################################################################
#### 20.4 COMPARACIÓN DE MÉTRICAS DE DESEMPEÑO DE LOS TRES MODELOS
##################################################################


# Empezamos comparando por la precisión global ("accuracy")
# Creamos un dataframe en que las entradas de la primera columna son 
# las identificaciones de los modelos


accuracy_modelos <- data.frame(
  Modelo = c(
    "SVM",
    "Arbol_decision",
    "Naive_Bayes"
  ),
  
  # Y las de la segunda los valores de "Accuracy" extraidos
  # del índice ["Accuracy"] de $overall de la 'confusionMatrix'
  # de cada modelo  
  
  Accuracy = c(
    metricasclases_svm$overall["Accuracy"],
    metricasclases_arbol$overall["Accuracy"],
    metricasclases_nb$overall["Accuracy"]
  )
)

# Mostramos el dtaframe
accuracy_modelos


# Ahora repetimos la misma filosofía de procedimiento, pero para extraer y comparar todas las métricas pedidas:

# --------------------------------------------------
# TABLA COMPARATIVA DE MÉTRICAS DE LOS MODELOS
# --------------------------------------------------

# Creamos un data frame con las métricas de evaluación
metricas_modelos <- data.frame(
  
  # Nombre de los modelos evaluados
  Modelo = c(
    "SVM",
    "Arbol_decision",
    "Naive_Bayes"
  ),
  
  Accuracy = c(
    metricasclases_svm$overall["Accuracy"],
    metricasclases_arbol$overall["Accuracy"],
    metricasclases_nb$overall["Accuracy"]
  ),
  
  # El resto de métricas se extraen de sus respectivos índices 
  # en la $byClass de la 'confusionMatrix'de cada modelo  
  
  Precision = c(
    mean(metricasclases_svm$byClass[, "Pos Pred Value"], na.rm = TRUE),
    mean(metricasclases_arbol$byClass[, "Pos Pred Value"], na.rm = TRUE),
    mean(metricasclases_nb$byClass[, "Pos Pred Value"], na.rm = TRUE)
  ),
  
  Sensibilidad = c(
    mean(metricasclases_svm$byClass[, "Sensitivity"], na.rm = TRUE),
    mean(metricasclases_arbol$byClass[, "Sensitivity"], na.rm = TRUE),
    mean(metricasclases_nb$byClass[, "Sensitivity"], na.rm = TRUE)
  ),
  
  Especificidad = c(
    mean(metricasclases_svm$byClass[, "Specificity"], na.rm = TRUE),
    mean(metricasclases_arbol$byClass[, "Specificity"], na.rm = TRUE),
    mean(metricasclases_nb$byClass[, "Specificity"], na.rm = TRUE)
  ),
  
  
  F1_Score = c(
    mean(metricasclases_svm$byClass[, "F1"], na.rm = TRUE),
    mean(metricasclases_arbol$byClass[, "F1"], na.rm = TRUE),
    mean(metricasclases_nb$byClass[, "F1"], na.rm = TRUE)
  )
)

# Mostramos la tabla
metricas_modelos



# También podemos ver las métricas de desempeño para cada clase y modelo

metricasclases_svm$byClass
metricasclases_arbol$byClass
metricasclases_nb$byClass

