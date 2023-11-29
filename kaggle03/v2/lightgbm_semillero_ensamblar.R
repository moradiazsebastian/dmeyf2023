library(data.table)
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

PARAM <- list()
PARAM$experimento <- "0009_semillero_competencia_baseline_lags_4_goss"
PARAM$input$dataset <- "./datasets/competencia_03_lag_delta_4.csv.gz"
PARAM$input$prediccion <- "prediccion.txt"
PARAM$input$ganancias <- "ganancias.csv"
PARAM$input$ensamble <- "ensamble_probabilidades.csv"

PARAM$POS_ganancia <- 273000
PARAM$NEG_ganancia <- -7000

PARAM$input$future <- c(202109) # meses donde se aplica el modelo
set.seed(42)
PARAM$input$semillas <- sample(10000:100000, 20, replace = FALSE)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# lectura del dataset
setwd("~/buckets/b1")
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# lectura de semillas. requiere correr lightgbm_semillero.r
setwd(paste0("~/buckets/b1/exp/",PARAM$experimento))
dapply <- dataset[foto_mes == PARAM$input$future]


# definimos columnas para trabajar posteriormente 
columnas_id <- c("numero_de_cliente", "foto_mes")
columnas_probabilidad <- as.character(PARAM$input$semillas)
columna_avg <- c("prob")
columna_ternaria <- c("clase_ternaria")



for(ksemilla in PARAM$input$semillas){
  # leemos las probabilidades de la semilla
  setwd(paste0("./",ksemilla))
  prediccion <- fread(PARAM$input$prediccion, sep = "\t", header=TRUE)

  # hacemos merge entre el dataset de test y las predicciones. 
  # los datasets tienen que coincidir en dimensiones
  prediccion[, (paste0(ksemilla)) := prediccion$prob]
  prediccion <- subset(prediccion, select = -prob)
  
  dapply <- merge(dapply, prediccion, by = columnas_id, all.x = TRUE)
  
  # volvemos al wd anterior
  setwd("..")
}


# ensamblamos calculando el promedio de las semillas.
dapply[, prob := rowMeans(.SD), .SDcols = columnas_probabilidad]
ensamble <- dapply[, c(columnas_id,columnas_probabilidad,columna_avg,columna_ternaria), with=FALSE]

# obtenemos archivo con ganancias de las semillas
setwd(paste0("~/buckets/b1/exp/",PARAM$experimento))
ganancia_final <- fread(PARAM$input$ganancias, stringsAsFactors = TRUE)

#obtengo probabilidades promedio y ordeno por probabilidad descendente
tb_entrega <- ensamble[, list(numero_de_cliente, foto_mes, clase_ternaria, prob)]
setorder(tb_entrega, -prob)

# genero archivos con los  "envios" mejores
# deben subirse "inteligentemente" a Kaggle para no malgastar submits
# si la palabra inteligentemente no le significa nada aun
# suba TODOS los archivos a Kaggle
# espera a la siguiente clase sincronica en donde el tema sera explicado

dir.create("./ensamble/", showWarnings = FALSE)
setwd("ensamble")

cortes <- seq(8000, 18000, by = 50)
ganancia_semilla <- data.table()
for (envios in cortes) {
  tb_entrega[, Predicted := 0L]
  tb_entrega[1:envios, Predicted := 1L]
  
  
  fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
         file = paste0(envios, ".csv"),
         sep = ","
  )
  
  ganancia_envios <- list(
    semilla = "ensamble",
    envios = envios,
    ganancia = PARAM$NEG_ganancia*nrow(tb_entrega[Predicted==1 & clase_ternaria!="BAJA+2"]) + PARAM$POS_ganancia*nrow(tb_entrega[Predicted==1 & clase_ternaria=="BAJA+2"])
  )
  
  #acumulamos las ganancias de todos los envios en un solo data table
  ganancia_semilla <- rbind(ganancia_semilla, ganancia_envios)
}

#acumulamos las ganancias de todos los envios, y todas las semillas. 
ganancia_final <- rbindlist(list(ganancia_final, ganancia_semilla))

setwd(paste0("~/buckets/b1/exp/",PARAM$experimento))

fwrite(ensamble,
       file = paste0(PARAM$input$ensamble),
       sep = ","
)

fwrite(ganancia_final,
       file = paste0("ganancias.csv"),
       sep = ","
)


cat("\n\nPromedio de probabilidades en el semillero finalizado\n")


