# run the model with built-in data, these codes can run directly if package installed
library("SHAPforxgboost")
library("data.table")
library("ggplot2")

PARAM <- list()
PARAM$experimento <- "EXP-COLAB-BO-55-centroids-FINAL"
PARAM$input$dataset <- "contribuciones_apply.csv.gz"
PARAM$input$shap_values <- "contribuciones.csv.gz"

#setwd("~/buckets/b1/")
setwd("~/FacuDMEF/")

# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- fread(PARAM$input$shap_values, stringsAsFactors = TRUE)
dataX <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# To prepare the long-format data:
shap_long <- shap.prep(shap_contrib = shap_values[, -c("SHAP_VAL", "cluster")], X_train = dataX, top_n = 150)
# (Notice that there will be a data.table warning from `melt.data.table` due to `dayint` coerced from
# integer to double)

# **SHAP summary plot**
shap.plot.summary(shap_long)
ggsave("shap_summary.png", height = 150, width = 50, units = "cm", limitsize = FALSE)

plot_data <- shap.prep.stack.data(shap_contrib = shap_values[, -c("SHAP_VAL", "cluster")], top_n = 20, n_groups = 7)
shap.plot.force_plot_bygroup(plot_data)
ggsave("shap_force_plot_bygroup.png", height = 15, width = 40, units = "cm", limitsize = FALSE)

plot_data <- shap.prep.stack.data(shap_contrib = shap_values[, -c("SHAP_VAL", "cluster")], top_n = 10, n_groups = 7)
shap.plot.force_plot_bygroup(plot_data)
ggsave("shap_force_plot_bygroup-10.png", height = 15, width = 40, units = "cm", limitsize = FALSE)