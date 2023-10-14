# Este script genera clustering en bajas y los graficos de las variables

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")
require("matrixStats")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$dataset_clusters <- "./exp/RF-CLUST0001/RF-CLUST0001_competencia_02_b2_clusters.csv.gz"
PARAM$table_output <- "competencia_02_b2_clusters_table.csv"
PARAM$experimento <- "RF-CLUST0003"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

setwd("~/buckets/b1/")
set.seed(501593)

print("Cargando dataset")
# cargo el dataset
dataset <- fread(PARAM$dataset) 
dataset_clusters <- fread(PARAM$dataset_clusters)

print("Haciendo transformaciones")
dataset[dataset_clusters, on = "numero_de_cliente", cluster := i.cluster]
dataset <- dataset[clase_ternaria == "BAJA+2", ]

# ordeno el dataset
dataset <- dataset[order(numero_de_cliente, foto_mes), ]

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria", "cluster")
)

dataset[, cluster_name := as.character(cluster)]

options(warn = -1)

print("Calculando medias")
counts <- dataset[, .(cluster_name, count = .N), by = cluster_name]

for (j in campos_buenos) {
  set(dataset, which(is.na(dataset[[j]])), j, 0)
}

output_ds <- dataset[, lapply(.SD, mean), by = cluster_name, .SDcols = campos_buenos]
output_ds[counts, on = "cluster_name", cluster_size := i.count]
output_ds <- output_ds[order(cluster_name), ]

clusters <- unique(output_ds$cluster_name)

output_ds <- transpose(output_ds, keep.names = "variable", make.names = "cluster_name")
output_ds[, Mean := rowMeans(.SD, na.rm = TRUE), .SDcols = clusters]
output_ds[, Median := rowMedians(as.matrix(.SD), na.rm = TRUE), .SDcols = clusters]
output_ds[, Max := rowMaxs(as.matrix(.SD), na.rm = TRUE), .SDcols = clusters]
output_ds[, Min := rowMins(as.matrix(.SD), na.rm = TRUE), .SDcols = clusters]

clusters_mean_dists <- lapply(clusters, (function (x) paste0("MeanDist", x)))
clusters_med_dists <- lapply(clusters, (function (x) paste0("MedianDist", x)))

output_ds[, unlist(clusters_mean_dists) := abs(.SD - Mean), .SDcols = clusters]
output_ds[, unlist(clusters_med_dists) := abs(.SD - Median), .SDcols = clusters]

clusters_norm_mean_dists <- lapply(clusters, (function (x) paste0("NMeanDist", x)))
clusters_norm_med_dists <- lapply(clusters, (function (x) paste0("NMedianDist", x)))

output_ds[, unlist(clusters_norm_mean_dists) := (.SD - Min) / (Max - Min), .SDcols = clusters_mean_dists]
output_ds[, unlist(clusters_norm_med_dists) := (.SD - Min) / (Max - Min), .SDcols = clusters_med_dists]

fwrite(output_ds, file = PARAM$table_output)
print("Listo")