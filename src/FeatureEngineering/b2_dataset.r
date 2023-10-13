# Este script genera clustering en bajas y los graficos de las variables

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$output_dataset <- "./datasets/competencia_02_b2.csv.gz"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("~/buckets/b1/")


# cargo el dataset
dataset <- fread(PARAM$dataset) # donde entreno
dataset <- dataset[clase_ternaria == "BAJA+2", ]

# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

# guardo el dataset
fwrite(dataset, file = PARAM$output_dataset)