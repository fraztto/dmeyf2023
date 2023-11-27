library("data.table")

PARAM <- list()
PARAM$experimento_input <- "HLR-C3-100S-1"
PARAM$experimento_input_ds <- "predicciones_modelo_"
PARAM$experimento_output <- "HLR-C3-100S-1-ensamble"

base_path <- "~/buckets/b1"
base_path <- "~/FacuDMEF"
setwd(base_path)
setwd(paste0("./exp/", PARAM$experimento_input, "/"))

# list all files in the directory
files <- list.files(pattern = PARAM$experimento_input_ds)
for (i in 1:length(files)) {
  new_dataset <- fread(files[i], stringsAsFactors = TRUE)
  if (i == 1) {
    predictions_dataset <- new_dataset
  } else {
    new_col <- grep("model_", names(new_dataset), value = TRUE)
    predictions_dataset[, (new_col) := new_dataset[, ..new_col]]
  }
}
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
