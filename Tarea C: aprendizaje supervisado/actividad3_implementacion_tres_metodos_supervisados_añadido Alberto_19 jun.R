# Código de R para analizar los métodos supervisados y poder realizar las preguntas:

############################################################
#### 3. IMPLEMENTACIÓN DE MÉTODOS DE APRENDIZAJE SUPERVISADO
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

# Después cargamos tanto los datos escalados, que usaremos tanto para el modelo 
# SVM como para Naïve Bayes, como los datos filtrados que usaremos para el 
# árbol de decisión, ya que no necesita escalado.  (*esto ya esta hecho en el 
# script de preparacion de datos, pero yo lo tengo que hacer para cargar los 
# archivos en mi script de R*)

datos_escalados <- read.csv("datos_escalados.csv")
datos_filtrados <- read.csv("datos_filtrados.csv")

# Visualizamos los datos que tenemos
names(datos_escalados)
dim(datos_escalados)
head(datos_escalados)

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
#### 3.1 SVM
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

# Por último, realizamos la matriz de confusión

matrizconfusion_svm <- confusionMatrix(
  predicion_svm,
  test_escalado$Clase
)

matrizconfusion_svm

############################################################
#### 3.2 ÁRBOL DE DECISIÓN
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

# Matriz de confusión
matrizconfusion_arbol <- confusionMatrix(
  predicion_arbol,
  test_filtrado$Clase
)

matrizconfusion_arbol

# Dibujamos árbol de decisión
rpart.plot(modelo_arbol)

############################################################
#### 3.3 NAIVE BAYES
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

matrizconfusion_nb <- confusionMatrix(
  predicion_nb,
  test_escalado$Clase
)

matrizconfusion_nb


##################################################################
#### 3.4 COMPARACIÓN DE MÉTRICAS DE DESEMPEÑO DE LOS TRES MODELOS
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
    matrizconfusion_svm$overall["Accuracy"],
    matrizconfusion_arbol$overall["Accuracy"],
    matrizconfusion_nb$overall["Accuracy"]
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
    matrizconfusion_svm$overall["Accuracy"],
    matrizconfusion_arbol$overall["Accuracy"],
    matrizconfusion_nb$overall["Accuracy"]
  ),
  
  # El resto de métricas se extraen de sus respectivos índices 
    # en la $byClass de la 'confusionMatrix'de cada modelo  
  
  Precision = c(
    mean(matrizconfusion_svm$byClass[, "Pos Pred Value"], na.rm = TRUE),
    mean(matrizconfusion_arbol$byClass[, "Pos Pred Value"], na.rm = TRUE),
    mean(matrizconfusion_nb$byClass[, "Pos Pred Value"], na.rm = TRUE)
  ),
  
  Sensibilidad = c(
    mean(matrizconfusion_svm$byClass[, "Sensitivity"], na.rm = TRUE),
    mean(matrizconfusion_arbol$byClass[, "Sensitivity"], na.rm = TRUE),
    mean(matrizconfusion_nb$byClass[, "Sensitivity"], na.rm = TRUE)
  ),
  
  Especificidad = c(
    mean(matrizconfusion_svm$byClass[, "Specificity"], na.rm = TRUE),
    mean(matrizconfusion_arbol$byClass[, "Specificity"], na.rm = TRUE),
    mean(matrizconfusion_nb$byClass[, "Specificity"], na.rm = TRUE)
  ),
  
 
  F1_Score = c(
    mean(matrizconfusion_svm$byClass[, "F1"], na.rm = TRUE),
    mean(matrizconfusion_arbol$byClass[, "F1"], na.rm = TRUE),
    mean(matrizconfusion_nb$byClass[, "F1"], na.rm = TRUE)
  )
)

# Mostramos la tabla
metricas_modelos




# También odemos ver las métricas de desempeño para cada clase y modelo

matrizconfusion_svm$byClass
matrizconfusion_arbol$byClass
matrizconfusion_nb$byClass

