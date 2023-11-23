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

features_string <- list()
for (cl in 1:7){
    v <- shap_values[cluster == cl,]
    cluster_dataX <- copy(dataX)
    cluster_dataX[, cluster := shap_values$cluster]
    cluster_dataX <- cluster_dataX[cluster == cl,]

    shap_long <- shap.prep(shap_contrib = v[, -c("SHAP_VAL", "cluster")], X_train = cluster_dataX[, -c("cluster")], top_n = 150)

    # **SHAP summary plot**
    shap.plot.summary(shap_long)
    ggsave(paste0("shap_summary-cluster-", cl, ".png"), height = 150, width = 50, units = "cm", limitsize = FALSE)

    top_n <- 20
    mean_values <- shap_long[, .(group_mean = mean(value)), by=variable]
    setorder(mean_values, -group_mean)
    features_string[[cl]] <- paste0(mean_values$variable[1:top_n], collapse = ",")
}

fwrite(data.table(cluster = 1:7, features = features_string), file = "features_string.csv", sep = "\t")
shap_long <- shap.prep(shap_contrib = shap_values[, -c("SHAP_VAL", "cluster")], X_train = dataX, top_n = 150)

# **SHAP summary plot**
shap.plot.summary(shap_long)
ggsave("shap_summary.png", height = 150, width = 50, units = "cm", limitsize = FALSE)

plot_data <- shap.prep.stack.data(shap_contrib = shap_values[, -c("SHAP_VAL", "cluster")], top_n = 20, n_groups = 7)
shap.plot.force_plot_bygroup(plot_data)
ggsave("shap_force_plot_bygroup.png", height = 15, width = 40, units = "cm", limitsize = FALSE)

plot_data <- shap.prep.stack.data(shap_contrib = shap_values[, -c("SHAP_VAL", "cluster")], top_n = 10, n_groups = 7)
shap.plot.force_plot_bygroup(plot_data)
ggsave("shap_force_plot_bygroup-10.png", height = 15, width = 40, units = "cm", limitsize = FALSE)

# find interactions between the best features in the cluster

# join all the possible variables:
all_features <- paste0(features_string, collapse = ",")
all_features <- unlist(strsplit(all_features, ","))
all_features <- unique(all_features)

# find the interactions between the best features
v <- shap_values[, ]
cluster_dataX <- copy(dataX)
cluster_dataX[, cluster := shap_values$cluster]
cluster_dataX <- cluster_dataX[cluster == cl,]


