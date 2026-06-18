############################################################
#### CONTINUACIÓN DEL SCRIPT: APRENDIZAJE NO SUPERVISADO
####
#### Este bloque continúa la numeración del script de procesamiento
#### (apartados 1 a 12) y utiliza los objetos generados en ese script:
#### datos_filtrados (sin escalar) y datos_escalados (centrados y escalados).
####
#### Justificación de las técnicas seleccionadas:
####
#### - PCA (reducción de dimensionalidad lineal): técnica clásica en datos
####   de expresión génica. Permite cuantificar qué porcentaje de la
####   variabilidad total se explica con pocas combinaciones lineales
####   de los 491 genes, y ofrece una representación fácilmente interpretable.
####
#### - t-SNE (reducción de dimensionalidad no lineal): complementa al PCA
####   porque no busca conservar la varianza global, sino las relaciones
####   de vecindad locales entre muestras. Permite detectar subgrupos que
####   el PCA no separa con claridad.
####
#### - k-means (clusterización basada en centroides): método sencillo y
####   eficiente, razonable como primera aproximación cuando se dispone de
####   una estimación a priori del número de grupos (aquí, cinco clases
####   biológicas).
####
#### - Clustering jerárquico (clusterización basada en un árbol): no exige
####   fijar a priori el número de clusters y permite visualizar mediante
####   un dendrograma la similitud entre muestras a distintos niveles.
############################################################


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
library(Rtsne)

### mclust nos permite calcular el índice de Rand ajustado (ARI), que
### utilizaremos para comparar de forma cuantitativa los clusters obtenidos
### con las clases biológicas reales.
library(mclust)

### 
library(dplyr)


############################################################
#### 14. CARGA DE LOS DATOS PROCESADOS
############################################################

### Seleccionamos el directorio de trabajo y cargamos los datos escalados
setwd("~/Máster en Bioinformática UNIR/Algoritmos e Inteligencia Artificial/Actividad 3")

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
  layout(title = "PCA 3D - Muestras coloreadas por clase biológica")

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
### idéntico al obtenido con k-means (0.1379), lo que indica una calidad
### interna de los clusters muy similar entre ambos métodos.

### La tabla de contingencia muestra un patrón parecido al de k-means: tres
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

