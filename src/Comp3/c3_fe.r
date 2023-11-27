# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

PARAM <- list()

PARAM$experimento <- "C3-FE-V2"
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

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
dataset[, tmobile_app := NA]

# Data Drifting

# Usamos rank para las monetarias
cols_monetarias <- c("mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","mcuenta_corriente_adicional","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_adicional","mcaja_ahorro_dolares","mcuentas_saldo","mautoservicio","mtarjeta_visa_consumo","mtarjeta_master_consumo","mprestamos_personales","mprestamos_prendarios","mprestamos_hipotecarios","mplazo_fijo_dolares","mplazo_fijo_pesos","minversion1_pesos","minversion1_dolares","minversion2","mpayroll","mpayroll2","mcuenta_debitos_automaticos","mttarjeta_visa_debitos_automaticos","mttarjeta_master_debitos_automaticos","mpagodeservicios","mpagomiscuentas","mcajeros_propios_descuentos","mtarjeta_visa_descuentos","mtarjeta_master_descuentos","mcomisiones_mantenimiento","mcomisiones_otras","mforex_buy","mforex_sell","mtransferencias_recibidas","mtransferencias_emitidas","mextraccion_autoservicio","mcheques_depositados","mcheques_emitidos","mcheques_depositados_rechazados","mcheques_emitidos_rechazados","matm","matm_other","Master_mfinanciacion_limite","Master_msaldototal","Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares","Master_mlimitecompra","Master_madelantopesos","Master_madelantodolares","Master_mpagado","Master_mpagospesos","Master_mpagosdolares","Master_mconsumototal","Master_mpagominimo","Visa_mfinanciacion_limite","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares","Visa_mconsumospesos","Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_mconsumototal","Visa_mpagominimo")
cols_monetarias_rank <- paste0("rank_", cols_monetarias)
dataset[, (cols_monetarias_rank) := lapply(.SD, function(x) frankv(x, na.last = TRUE)), by = foto_mes, .SDcols = cols_monetarias]


# Feature Engineering Historico  ----------------------------------------------

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


#   aqui deben calcularse los  lags y  lag_delta
cols <- names(dataset)
cols <- cols[!cols %in% c("numero_de_cliente", "foto_mes", "clase_ternaria")]

numeric_cols <- names(Filter(is.numeric, dataset))
numeric_cols <- numeric_cols[!numeric_cols %in% c("numero_de_cliente", "foto_mes", "clase_ternaria")]

# iterar todos los lags hasta 6
for (i in c(1:6)) {
  # lag
  # add name to the columns with the lag number
  anscols <- paste("lag", i, cols, sep="_")

  dataset[, (anscols) := shift(.SD, i, NA, "lag"), .SDcols=cols, by = numero_de_cliente]
}

for (j in numeric_cols) {
  anscols <- paste("lag_delta", 1, j, sep="_")
  dataset[, (anscols) := get(j) - get(paste0("lag_1_", j))]
  dataset[, (paste0("avg6_", j)) := ( get(j) + get(paste("lag_1_", j)) + get(paste("lag_2_", j)) + get(paste("lag_3_", j)) + get(paste("lag_4_", j)) + get(paste("lag_5_", j)) )/6, .SDcols = numeric_cols, by = numero_de_cliente]
  dataset[, (paste0("avg3_", j)) := (get(j) + get(paste("lag_1_", j)) + get(paste("lag_2_", j)))/3, .SDcols = numeric_cols, by = numero_de_cliente]
}

print("Termine transformaciones")

fwrite(dataset, file = "competencia_03_fe_v2.csv", sep = ",")