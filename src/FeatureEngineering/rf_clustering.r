# Este script genera clustering en bajas y los graficos de las variables

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")
require("randomForest")
require("umap")
require("ggplot2")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_02_b2.csv.gz"
PARAM$dataset_salida <- "competencia_02_b2_clusters.csv.gz"
PARAM$low_dim_plot <- "competencia_02_b2_clusters_low_dim_map.png"
PARAM$experimento <- "RF-CLUST0001"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("~/buckets/b1/")
set.seed(501593)


# cargo el dataset
dataset <- fread(PARAM$dataset) # donde entreno

# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

# me quedo con las bajas

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)

ds_fixed <- na.roughfix(dataset[, campos_buenos, with = FALSE])
rf_fit <- randomForest(x = ds_fixed,
                       y = NULL, ntree = 100, proximity = TRUE, oob.prox = TRUE,
                       na.action = na.roughfix)

hclust_rf <- hclust(as.dist(1 - rf_fit$proximity), method = "ward.D2")
rf_cluster <- cutree(hclust_rf, k = 7)

dataset$cluster <- rf_cluster

fwrite(dataset[, .(numero_de_cliente, foto_mes, cluster)], file = paste0(PARAM$experimento, "_", PARAM$dataset_salida))

# Aplicar un umap para graficar clusters
custom_config <- umap.defaults
custom_config$random_state <- 831431
custom_config$n_components <- 2
custom_config$n_neighbors <- 50

rf_map <- umap(
  1 - rf_fit$proximity,
  config = custom_config
)
print("Guardando el dataset con el cluster")

# ggplot para graficar los clusters en un scatterplot
plt <- ggplot(as.data.table(rf_map$layout), aes(x = V1, y = V2, color = as.factor(rf_cluster))) +
  geom_point(alpha = 0.5, size = 2.5) +
  ggtitle("RF Clustering") +
  theme_minimal() +
  theme(legend.position = "none")
png(PARAM$low_dim_plot, width = 1024, height = 768, units = "px")
print(plt)
dev.off()