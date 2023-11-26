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
PARAM$experimento <- "HLR-C3-100S-1"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202107, 202106, 202105, 202104, 202103, 202102, 202101, 202012,
                          202011, 202010, 202009, 202008, 202007, 202006, 202005, 202004,
                          202003, 202002, 202001, 201912, 201911, 201910, 201909, 201908,
                          201907, 201906, 201905, 201904, 201903, 201902, 201901)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

PARAM$finalmodel$semillas <- c( 110017, 110023, 110039, 110051, 110059, 110063, 110069, 110083, 110119, 110129, 110161, 110183, 110221, 110233, 110237, 110251, 110261, 110269, 110273, 110281, 110291, 110311, 110321, 110323, 110339, 110359, 110419, 110431, 110437, 110441, 110459, 110477, 110479, 110491, 110501, 110503, 110527, 110533, 110543, 110557, 110563, 110567, 110569, 110573, 110581, 110587, 110597, 110603, 110609, 110623, 110629, 110641, 110647, 110651, 110681, 110711, 110729, 110731, 110749, 110753, 110771, 110777, 110807, 110813, 110819, 110821, 110849, 110863, 110879, 110881, 110899, 110909, 110917, 110921, 110923, 110927, 110933, 110939, 110947, 110951, 110969, 110977, 110989, 111029, 111031, 111043, 111049, 111053, 111091, 111103, 111109, 111119, 111121, 111127, 111143, 111149, 111187, 111191, 111211, 111217)

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 20
PARAM$finalmodel$optim$learning_rate <- 1
PARAM$finalmodel$optim$feature_fraction <- 0.50
PARAM$finalmodel$optim$min_data_in_leaf <- 106
PARAM$finalmodel$optim$num_leaves <- 308


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
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
  neg_bagging_fraction = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


print("Haciendo transformaciones")
# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]
dataset[foto_mes %in% c(201905, 201910, 202006), c("mrentabilidad", "mrentabilidad_annual", "mcomisiones", "mactivos_margen", "mpasivos_margen", "ccomisiones_otras", "mcomisiones_otras") := NA]
dataset[foto_mes %in% c(201904), c("mttarjeta_visa_debitos_automaticos") := NA]
dataset[foto_mes %in% c(201901, 201902, 201903, 201904, 201905), c("ctransferencias_recibidas", "mtransferencias_recibidas") := NA]
dataset[foto_mes %in% c(201910), c("chomebanking_transacciones") := NA]
dataset[foto_mes == 202006, names(dataset) := NA]
dataset[, tmobile_app := NA]

# Data Drifting

# Usamos rank para las monetarias
cols_monetarias <- c("mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mcuentas_saldo","mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios","mplazo_fijo_dolares","mplazo_fijo_pesos","minversion1_pesos","minversion1_dolares","minversion2","mpayroll","mpayroll2","mcuenta_debitos_automaticos","mttarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos","mcomisiones_mantenimiento","mcomisiones_otras","mforex_buy","mforex_sell","mtransferencias_recibidas","mtransferencias_emitidas","mextraccion_autoservicio","mcheques_depositados","mcheques_emitidos","mcheques_depositados_rechazados","mcheques_emitidos_rechazados","matm","matm_other","Master_mfinanciacion_limite","Master_msaldototal","Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares","Master_mlimitecompra","Master_madelantopesos","Master_madelantodolares","Master_mpagado","Master_mpagospesos","Master_mpagosdolares","Master_mconsumototal","Master_mpagominimo","Visa_mfinanciacion_limite","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares","Visa_mconsumospesos","Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_mconsumototal","Visa_mpagominimo")
cols_monetarias_rank <- paste0("rank_", cols_monetarias)
dataset[, (cols_monetarias_rank) := lapply(.SD, function(x) frankv(x, na.last = TRUE)), by = foto_mes, .SDcols = cols_monetarias]


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
cols <- names(dataset)
cols <- cols[!cols %in% c("numero_de_cliente", "foto_mes", "clase_ternaria")]

numeric_cols <- names(Filter(is.numeric, dataset))
numeric_cols <- numeric_cols[!numeric_cols %in% c("numero_de_cliente", "foto_mes", "clase_ternaria")]

# iterar todos los lags hasta 6
for (i in 1:6) {
  # lag
  # add name to the columns with the lag number
  anscols <- paste("lag", i, cols, sep="_")

  dataset[, (anscols) := shift(.SD, i, NA, "lag"), .SDcols=cols]

  # lag_delta
  if (i == 1) {
    anscols <- paste("lag_delta", i, numeric_cols, sep="_")
    dataset[, (anscols) := .SD - shift(.SD, i, 0, "lag"), .SDcols=numeric_cols]
  }
  else if (i < 6) {
    lagcols = paste("lag", i - 1, numeric_cols, sep="_")
    lag1cols = paste("lag", i, numeric_cols, sep="_")
    anscols = paste("lag_delta", i, numeric_cols, sep="_")

    dataset[, (anscols) := .SD - mget(lag1cols) , .SDcols=lagcols]
  }
}

for(col in numeric_cols) {
  lags <- 1:6
  lag_cols <- paste("lag", lags, col, sep="_")
  mean_col <- paste("mean", col, sep="_")
  dataset[, (mean_col) := rowMeans(.SD, na.rm = TRUE), .SDcols=lag_cols]
}

# Features a mano
dataset[, ccomisiones_otras_sum_cpayroll_trx := ccomisiones_otras + cpayroll_trx]
dataset[, catm_trx_sum_ccomisiones_otras := catm_trx + ccomisiones_otras]
dataset[, catm_trx_sum_cproductos := catm_trx + cproductos]
dataset[, ctarjeta_debito_transacciones_sum_cproductos := ctarjeta_debito_transacciones + cproductos]
dataset[, ctrx_quarter_sum_mpagomiscuentas := ctrx_quarter + mpagomiscuentas]
dataset[, visa_mlimitecompra_multiply_ctrx_quarter := Visa_mlimitecompra * ctrx_quarter]
dataset[, cextraccion_autoservicio_sum_ccomisiones_otras := cextraccion_autoservicio + ccomisiones_otras]
dataset[, ctrx_quarter_sum_mplazo_fijo_dolares := ctrx_quarter + mplazo_fijo_dolares]
dataset[, mextraccion_autoservicio_sum_ccomisiones_otras := mextraccion_autoservicio + ccomisiones_otras]
dataset[, ctarjeta_debito_transacciones_diff_ctrx_quarter := ctarjeta_debito_transacciones - ctrx_quarter]
dataset[, ccomisiones_otras_sum_ctarjeta_debito_transacciones := ccomisiones_otras + ctarjeta_debito_transacciones]
dataset[, master_fvencimiento_ratio_ctrx_quarter := Master_Fvencimiento / ctrx_quarter]
dataset[, matm_sum_ccomisiones_otras := matm + ccomisiones_otras]
dataset[, master_fechaalta_ratio_ctrx_quarter := Master_fechaalta / ctrx_quarter]

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

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]
predicciones <- dapply[, list(numero_de_cliente, foto_mes)]

for (semilla in PARAM$finalmodel$semillas){
  print(paste0("Semilla: ", semilla))
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)
  param_completo$seed <- semilla
  # entreno el modelo
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  # imprimo importancia de variables del modelo de esta semilla
  tb_importancia <- as.data.table(lgb.importance(modelo))

  # Configuro nombre del archivo
  archivo_importancia <- paste0("importance_", semilla, ".txt")

  # Guardo en el archivo
  fwrite(tb_importancia,
  file = archivo_importancia,
  sep = "\t"
  )

  # Aplico el modelo a los nuevos datos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )

  col_name <- paste0("model_", semilla)
  predicciones[, (col_name) := prediccion]
}

# grabo las probabilidad de los modelos
fwrite(predicciones,
  file = "predicciones.csv.gz",
  sep = "\t"
)
