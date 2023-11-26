library("data.table")

PARAM <- list()
PARAM$experimento_input <- "BO-C3-20S-50IT-1-IT45"
PARAM$experimento_input_ds <- "predicciones.csv.gz"
PARAM$experimento_output <- "BO-C3-20S-50IT-1-IT45-ensamble"

base_path <- "~/buckets/b1"
setwd(base_path)
setwd(paste0("./exp/", PARAM$experimento_input, "/"))

predictions_dataset <- fread(PARAM$experimento_input_ds, stringsAsFactors = TRUE)
prediction_cols <- grep("model_", names(predictions_dataset), value = TRUE)
predictions_dataset[, prediction_mean := rowMeans(.SD), .SDcols = prediction_cols]
predictions_dataset[, prediction_median := apply(.SD, 1, median), .SDcols = prediction_cols]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
setwd(base_path)
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento_output, "/"), showWarnings = FALSE)

setwd(paste0("./exp/", PARAM$experimento_output, "/"))

setorder(predictions_dataset, -prediction_mean)
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  predictions_dataset[, Predicted := 0L]
  predictions_dataset[1:envios, Predicted := 1L]

  fwrite(predictions_dataset[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento_output, "_mean_", envios, ".csv"),
    sep = ","
  )
}

setorder(predictions_dataset, -prediction_median)
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  predictions_dataset[, Predicted := 0L]
  predictions_dataset[1:envios, Predicted := 1L]

  fwrite(predictions_dataset[, list(numero_de_cliente, Predicted)],
    file = paste0(PARAM$experimento_output, "_median_", envios, ".csv"),
    sep = ","
  )
}
