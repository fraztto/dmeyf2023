library("data.table")
library("dplyr")
library("ggplot2")


PARAM <- list()
PARAM$experimento_1 <- "EXP-COLAB-BO-54-FINAL"
PARAM$pattern_1 <- "EXP-COLAB-BO-54-FINAL_"
PARAM$experimento_2 <- "EXP-COLAB-BO-54-WITH-CENTROIDS-FINAL"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"
PARAM$input$future <- c(202107)

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Establezco el Working Directory DEL EXPERIMENTO
setwd("~/buckets/b1")
#setwd("~/FacuDMEF/")


EstimarGanancia <- function(preds, dataset_test) {
    # hago la union de los parametros basicos y los moviles que vienen en x
    setorder(dataset_test, numero_de_cliente)
    setorder(preds, numero_de_cliente)

    tbl <- copy(dataset_test[, list("gan" = ifelse(clase_ternaria == "BAJA+2",
        PARAM$hyperparametertuning$POS_ganancia,
        PARAM$hyperparametertuning$NEG_ganancia
    ))])

    tbl[, ganancia := preds$Predicted * gan]
    ganancia <- sum(tbl$ganancia)
    return(ganancia)
}

# cargo el dataset donde se entrena
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)[foto_mes %in% PARAM$input$future]

# list files in folder
paths1 <- list.files(path = paste0("./exp/", PARAM$experimento_1, "/"), pattern = "*.csv", full.names = TRUE)
filenames1 <- list.files(path = paste0("./exp/", PARAM$experimento_1, "/"), pattern = "*.csv", full.names = FALSE)

# extraer envios del nombre del archivo
envios1 <- filenames1 %>%
    stringr::str_replace_all(pattern = PARAM$pattern_1, replacement = "") %>%
    stringr::str_replace_all(pattern = ".csv", replacement = "") %>%
    as.numeric()

ganancias <- data.table(tratamiento = integer(), envios = integer(), ganancia = numeric())
for (i in 1:length(paths1)) {
    # cargo las predicciones
    prediccion <- fread(paths1[i], stringsAsFactors = TRUE)
    # calculo la ganancia
    ganancia <- EstimarGanancia(prediccion, dataset)

    # guardo la ganancia
    ganancias <- rbind(ganancias, data.table(tratamiento = 0, envios = envios1[i], ganancia = ganancia))
}


# list files in folder
paths2 <- list.files(path = paste0("./exp/", PARAM$experimento_2, "/"), pattern = "*.csv", full.names = TRUE)
filenames2 <- list.files(path = paste0("./exp/", PARAM$experimento_2, "/"), pattern = "*.csv", full.names = FALSE)

# extraer envios del nombre del archivo
envios2 <- filenames2 %>%
    stringr::str_replace_all(pattern = PARAM$pattern_2, replacement = "") %>%
    stringr::str_replace_all(pattern = ".csv", replacement = "") %>%
    as.numeric()

ganancias <- data.table(tratamiento = integer(), envios = integer(), ganancia = numeric())
for (i in 1:length(paths2)) {
    # cargo las predicciones
    prediccion <- fread(paths2[i], stringsAsFactors = TRUE)
    # calculo la ganancia
    ganancia <- EstimarGanancia(prediccion, dataset)

    # guardo la ganancia
    ganancias <- rbind(ganancias, data.table(tratamiento = 1, envios = envios2[i], ganancia = ganancia))
}

# plot de las ganancias
ggplot(ganancias, aes(x = envios, y = ganancia, color = factor(tratamiento))) +
    geom_line() +
    geom_point() +
    theme_bw() +
    labs(title = "Ganancia por envio", x = "Envios", y = "Ganancia") +
    scale_color_discrete(name = "Tratamiento", labels = c("Sin centroides", "Con centroides")) +
    theme(legend.position = "bottom")

ggsave("ganancia.png", height = 15, width = 40, units = "cm", limitsize = FALSE)


# apply a wilcoxon test to the ganancias
wilcox.test(ganancia ~ tratamiento, data = ganancias)
