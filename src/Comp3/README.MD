### Para correr el modelo entregado:

1. Optimización bayesiana:
Correr el archivo c3_lightgbm_binaria_BO.r
Esto va a generar la optimización bayesiana con 50 iteraciones, desde este resultado tomamos la iteración 39:

```
num_iterations <- 1025
learning_rate <- 0.031725083
feature_fraction <- 0.736456953
min_data_in_leaf <- 3300
num_leaves <- 131
```

2. Entrenamiento de ensamble:
Correr el archivo c3_ensamble.r
Este script genera un archivo combinando las predicciones del ensamble de 20 semillas y las predicciones de cada una de las observaciones

3. Union del ensamble:
Correr el archivo c3_ensamble_combine.r
Este script toma la salida del script anterior y crea los archivos con las predicciones para kaggle como ya hemos hecho otras veces pero en este caso se hace la combinación de scores con media y con mediana. La entrega final se hizo con el ensamble por la media y 11000 envíos (BO-C3-20S-50IT-V2-IT39-ensamble_mean_11000.csv)