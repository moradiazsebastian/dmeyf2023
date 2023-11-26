library(ggplot2)
require("data.table")
require("stringr")

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


PARAM <- list()
PARAM$baseline <- "semillero_baseline"
PARAM$experimento <- "semillero_bagging_fraction_bagging_freq_neg_bagging_fraction_pos_bagging_fraction"
PARAM$input$ganancia <- "ganancias.csv"
PARAM$input$baseline_label <- "Baseline"
PARAM$input$experimento_label <- "bagging_fraction\nbagging_freq\nneg_bagging_fraction\npos_bagging_fraction"


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

#lectura ensemble. output del script lightgbm_semillero_ensamblar.r
setwd(paste0("~/buckets/b1/exp/",PARAM$baseline))
ganancias_final_baseline <- fread(PARAM$input$ganancia, stringsAsFactors = TRUE)

setwd(paste0("~/buckets/b1/exp/",PARAM$experimento))
ganancias_final_experimento <- fread(PARAM$input$ganancia, stringsAsFactors = TRUE)

# calcular desvio estandar para baseline. agregar label
ganancias_ensamble_baseline <- ganancias_final_baseline[semilla == "ensamble"]
ganancias_ensamble_baseline[, label := "Baseline"]

# calcular desvio estandar para experimento. agregar label
ganancias_ensamble_experimento <- ganancias_final_experimento[semilla == "ensamble"]
ganancias_ensamble_experimento[, label := PARAM$input$experimento_label]


baseline = subset(ganancias_ensamble_baseline, envios > 10000 & envios <15000)
experimento = subset(ganancias_ensamble_experimento, envios > 10000 & envios <15000)

manual_colors <- c()
manual_colors[PARAM$input$baseline_label] <- "blue"
manual_colors[PARAM$input$experimento_label] <- "red"

# graficar
ggplot() +
  geom_density(data = baseline, aes(x = ganancia, color = "Baseline", fill = "skyblue" ), alpha = 0.3, linetype = "solid") +
  geom_density(data = experimento, aes(x = ganancia, color = PARAM$input$experimento_label, fill = "lightcoral"),  alpha = 0.3, linetype = "dashed") +
  
  # Adding labels and title
  labs(x = "Ganancia", y = "Densidad", title = "Resultado experimento") +
  
  # Adding a legend
  scale_linetype_manual(values = c("solid", "dashed"), name = "Group") +
  scale_color_manual(values = manual_colors) +
  guides(fill = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "top", 
    legend.title=element_blank()
  )


# Perform Wilcoxon signed-rank test
wilcox_result <- wilcox.test(baseline$ganancia, experimento$ganancia, paired = TRUE)

# Print the test result
print(wilcox_result)