## Instrucciones

1. Usar el dataset de la materia preprocesado en la clase 1. (```competencia_01.csv```)
2. Con el script [```z401_BO_Public_oversampling.r```](z401_BO_Public_oversampling.r) , generar iterativamente los modelos
    2a. Este script espera de vuelta los scores que se obtienen en kaggle, puede usarse el log que se deja en la carpeta [```HT3990-OV```](HT3990-OV)
    2b. Si se decide subir a kaggle directamente, el script va a generar salidas (también adjuntas en la carpeta [```HT3990-OV```](HT3990-OV) ) que deben subirse a kaggle y escribir el resultado cuando el script lo pide.

### Notas:
- En caso de usar WSL es probable que el script no entienda el stdin para llenar el resultado, en ese caso, con el debugger, fui seteando el valor de resultado de Kaggle
- El script genera un archivo de log que se puede usar para ver los resultados de cada iteración

## Resultados

Los resultados están en el archivo [```BO_log.txt```](HT3990-OV/BO_log.txt) , el modelo final seleccionado fue el de la iteración **39** cuyo archivo ([```z399_039.csv``` ](HT3990-OV/z399_039.csv)) CSV de salida (junto a los demás) se incluye en la carpeta [```HT3990-OV```](HT3990-OV)