# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$output <- "./datasets/competencia_02_targets_b2.csv.gz"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/")

# cargo el dataset
dataset <- fread(PARAM$dataset) # donde entreno

# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

# obtengo todos los clientes unicos con alguna clase ternaria BAJA+2 

clientes_baja2 <- unique(dataset[clase_ternaria == "BAJA+2", numero_de_cliente])

# obtengo todos los registros de dichos clientes
dataset_baja2 <- dataset[numero_de_cliente %in% clientes_baja2]

# guardo el dataset nuevo
fwrite(dataset_baja2, PARAM$output)
