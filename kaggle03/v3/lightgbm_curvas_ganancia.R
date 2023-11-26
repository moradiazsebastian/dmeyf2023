library(ggplot2)
require("data.table")
require("stringr")

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


PARAM <- list()
PARAM$baseline <- "semillero_competencia_baseline_lags"
#PARAM$experimento <- "semillero_competencia_baseline_lags"
PARAM$input$ganancia <- "ganancias.csv"
PARAM$input$baseline_label <- "Baseline"
PARAM$input$experimento_label <- "bagging_freq\nneg_bagging_fraction"

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

#lectura ensemble. output del script lightgbm_semillero_ensamblar.r
setwd(paste0("~/buckets/b1/exp/",PARAM$baseline))
ganancias_final_baseline <- fread(PARAM$input$ganancia, stringsAsFactors = TRUE)

# setwd(paste0("~/buckets/b1/exp/",PARAM$experimento))
# ganancias_final_experimento <- fread(PARAM$input$ganancia, stringsAsFactors = TRUE)

# calcular desvio estandar para baseline. agregar label
ganancias_ensamble_baseline <- ganancias_final_baseline[semilla == "ensamble"]
ganancias_ensamble_baseline[, devest := sd(ganancia)]
ganancias_ensamble_baseline[, ganancia_min := ganancia-devest]
ganancias_ensamble_baseline[, ganancia_max := ganancia+devest]
ganancias_ensamble_baseline[, label := "Baseline"]

# calcular desvio estandar para experimento. agregar label
# ganancias_ensamble_experimento <- ganancias_final_experimento[semilla == "ensamble"]
# ganancias_ensamble_experimento[, devest := sd(ganancia)]
# ganancias_ensamble_experimento[, ganancia_min := ganancia-devest]
# ganancias_ensamble_experimento[, ganancia_max := ganancia+devest]
# ganancias_ensamble_experimento[, label := PARAM$input$experimento_label]

manual_colors <- c()
manual_colors[PARAM$input$baseline_label] <- "black"
# manual_colors[PARAM$input$experimento_label] <- "red"

# graficar
ggplot(ganancias_ensamble_baseline) +
  geom_line(aes(x=envios, y=ganancia, color = "Baseline"), linetype = "solid", size = 1) +
#  geom_ribbon(aes(x = envios, ymin = ganancia_min, ymax = ganancia_max), alpha = 0.2) +
  
  #geom_line(data = ganancias_ensamble_experimento, aes(x=envios, y=ganancia, color = PARAM$input$experimento_label),linetype = "solid", size = 1) +
  #geom_ribbon(data = ganancias_ensamble_experimento, aes(x = envios, ymin = ganancia_min, ymax = ganancia_max, fill = "lightcoral"), alpha = 0.2) +
  
  scale_color_manual(values = manual_colors) +
  
  labs(title = "Resultado experimento", x = "Envios", y = "Ganancia") +
  guides(fill = FALSE) +
  
  theme_minimal() + 
  theme(
    legend.position = "top", 
    legend.title=element_blank()
    )

print(ganancias_ensamble_baseline[which.max(ganancias_ensamble_baseline$ganancia), ])
