# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "EXP-COLAB-BO-31-centroids"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202101, 202102, 202103, 202104, 202105, 202106)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 501593

# hiperparametros BO 50it de experimentos colaborativos
PARAM$finalmodel$optim$num_iterations <- 266
PARAM$finalmodel$optim$learning_rate <- 0.118983015
PARAM$finalmodel$optim$feature_fraction <- 0.99756646
PARAM$finalmodel$optim$min_data_in_leaf <- 9579
PARAM$finalmodel$optim$num_leaves <- 1015

PARAM$clustering$k <- 7
PARAM$clustering$semilla <- 501593


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir gbdt o dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

dataset <- dataset[order(numero_de_cliente, foto_mes), ]

print("Haciendo transformaciones")
# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]
dataset[foto_mes %in% c(201905, 201910, 202006), c("mrentabilidad", "mrentabilidad_annual", "mcomisiones", "mactivos_margen", "mpasivos_margen", "ccomisiones_otras", "mcomisiones_otras") := NA]
dataset[foto_mes %in% c(201904), c("mttarjeta_visa_debitos_automaticos") := NA]
dataset[foto_mes %in% c(201901, 201902, 201903, 201904, 201905), c("ctransferencias_recibidas", "mtransferencias_recibidas") := NA]
dataset[foto_mes %in% c(201910), c("chomebanking_transacciones") := NA]
dataset[foto_mes == 202006, names(dataset) := NA]

# Data Drifting

# Usamos rank para las monetarias
# cols_monetarias <- c("mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mcuentas_saldo","mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios","mplazo_fijo_dolares","mplazo_fijo_pesos","minversion1_pesos","minversion1_dolares","minversion2","mpayroll","mpayroll2","mcuenta_debitos_automaticos","mttarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos","mcomisiones_mantenimiento","mcomisiones_otras","mforex_buy","mforex_sell","mtransferencias_recibidas","mtransferencias_emitidas","mextraccion_autoservicio","mcheques_depositados","mcheques_emitidos","mcheques_depositados_rechazados","mcheques_emitidos_rechazados","matm","matm_other","Master_mfinanciacion_limite","Master_msaldototal","Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares","Master_mlimitecompra","Master_madelantopesos","Master_madelantodolares","Master_mpagado","Master_mpagospesos","Master_mpagosdolares","Master_mconsumototal","Master_mpagominimo","Visa_mfinanciacion_limite","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares","Visa_mconsumospesos","Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_mconsumototal","Visa_mpagominimo")
# cols_monetarias_rank <- paste0("rank_", cols_monetarias)
# dataset[, (cols_monetarias_rank) := lapply(.SD, function(x) frankv(x, na.last = TRUE)), by = foto_mes, .SDcols = cols_monetarias]


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
cols <- names(dataset)
cols <- cols[!cols %in% c("numero_de_cliente", "foto_mes", "clase_ternaria")]

numeric_cols <- names(Filter(is.numeric, dataset))
numeric_cols <- numeric_cols[!numeric_cols %in% c("numero_de_cliente", "foto_mes", "clase_ternaria")]

# iterar todos los lags hasta 6
lags <- c(1, 3, 6)
for (i in lags) {
  # lag
  # add name to the columns with the lag number
  anscols <- paste("lag", i, cols, sep="_")

  dataset[, (anscols) := shift(.SD, i, NA, "lag"), .SDcols=cols]

  # lag_delta
  if (i == 1) {
    anscols <- paste("lag_delta", i, numeric_cols, sep="_")
    dataset[, (anscols) := .SD - shift(.SD, i, 0, "lag"), .SDcols=numeric_cols]
  }
  else if (i < 3) {
    lagcols = paste("lag", i - 1, numeric_cols, sep="_")
    lag1cols = paste("lag", i, numeric_cols, sep="_")
    anscols = paste("lag_delta", i, numeric_cols, sep="_")

    dataset[, (anscols) := .SD - mget(lag1cols) , .SDcols=lagcols]
  }
}

print("Termine transformaciones")

#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


# genero el modelo
param_completo <- c(PARAM$finalmodel$lgb_basicos,
  PARAM$finalmodel$optim)

modelo <- lgb.train(
  data = dtrain,
  param = param_completo,
)

#--------------------------------------
# ahora imprimo la importancia de variables
tb_importancia <- as.data.table(lgb.importance(modelo))
archivo_importancia <- "impo.txt"

fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
)

#--------------------------------------


# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

# aplico el modelo a los datos nuevos
prediccion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE]),
)

# busco los valores shap del mismo set de testing
contribucion <- predict(
  modelo,
  data.matrix(dapply[, campos_buenos, with = FALSE]),
  type = "contrib"
)

# hago clustering k-means de los shap values

set.seed(semilla)
kmeans <- kmeans(contribucion, PARAM$clustering$k, iter.max = 1000, nstart = 10)
centroides <- kmeans$centers

# grabo los centroides de los clusters
fwrite(centroides,
  file = "centroides.txt",
  sep = "\t"
)

# agrego las distancias a los centroides al dataset
#euclidean <- function(a, b) sqrt(sum((a - b)^2))

# nombre_centroides <- paste0("distancia_centroide_", 1:PARAM$clustering$k)
# dataset[train == 1L, (nombre_centroides) := euclidean(.SD, centroides), .SDcols = campos_buenos]

cat("\n\nLa generacion de los centroides ha terminado\n")
