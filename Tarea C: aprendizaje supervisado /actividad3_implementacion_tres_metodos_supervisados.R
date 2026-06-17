Código de R para analizar los métodos supervisados y poder realizar las preguntas:

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

trainIndex <- createDataPartition(
  datos_escalados$Clase,
  p = 0.8,
  list = FALSE
)

train_escalado <- datos_escalados[trainIndex, ]
test_escalado <- datos_escalados[-trainIndex, ]

train_filtrado <- datos_filtrados[trainIndex, ]
test_filtrado <- datos_filtrados[-trainIndex, ]

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

# Evaluamos la precisión y las métricas para la comparación de modelos

accuracy_modelos <- data.frame(
  Modelo = c(
    "SVM",
    "Arbol_decision",
    "Naive_Bayes"
  ),
  Accuracy = c(
    matrizconfusion_svm$overall["Accuracy"],
    matrizconfusion_arbol$overall["Accuracy"],
    matrizconfusion_nb$overall["Accuracy"]
  )
)

accuracy_modelos

# También podemos ver clase por clase cada dato para 
# las métricas de los 3 modelos diferntes para que sea 
#más fácil de comparar

matrizconfusion_svm$byClass
matrizconfusion_arbol$byClass
matrizconfusion_nb$byClass

