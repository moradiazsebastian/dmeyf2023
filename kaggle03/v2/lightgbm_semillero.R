# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$input$dataset <- "./datasets/competencia_03_lag_delta_2.csv.gz"
set.seed(42)
PARAM$input$semillas <- sample(10000:100000, 20, replace = FALSE)

#excluyendo 202006
#PARAM$input$training <- c(201902, 201903, 201904, 201905, 201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202002, 202003, 202004, 202005, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107)
#despues probar ccomo se debe, excluyendo 202106 y 202107
PARAM$input$training <- c(202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo



# PARAM$finalmodel$semilla <- 288913
PARAM$experimento <- "0010_semillero_competencia_baseline_lags_4_goss"

# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "goss", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  extra_trees = TRUE # Magic Sauce
  #  seed = PARAM$finalmodel$semilla
)
# hiperparametros optimos
PARAM$finalmodel$optim$num_iterations <- 860
PARAM$finalmodel$optim$learning_rate <- 0.0845
PARAM$finalmodel$optim$feature_fraction <- 0.29
PARAM$finalmodel$optim$min_data_in_leaf <- 6658
PARAM$finalmodel$optim$num_leaves <- 676
PARAM$finalmodel$optim$top_rate <- 0.273
PARAM$finalmodel$optim$other_rate <- 0.232
#PARAM$finalmodel$optim$bagging_freq <- 8
#PARAM$finalmodel$optim$neg_bagging_fraction <- 0.461 

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# como parte del analisis, se excluye 202006 del set de entrenamiento

# periodo 201905
dataset[foto_mes == 201905, mrentabilidad := NA]
dataset[foto_mes == 201905, mrentabilidad_annual := NA]
dataset[foto_mes == 201905, mcomisiones := NA]
dataset[foto_mes == 201905, mactivos_margen := NA]
dataset[foto_mes == 201905, mpasivos_margen := NA]
dataset[foto_mes == 201905, ccomisiones_otras := NA]
dataset[foto_mes == 201905, mcomisiones_otras := NA]

# periodo 201910
dataset[foto_mes == 201910, mrentabilidad := NA]
dataset[foto_mes == 201910, mrentabilidad_annual := NA]
dataset[foto_mes == 201910, mactivos_margen := NA]
dataset[foto_mes == 201910, mpasivos_margen := NA]
dataset[foto_mes == 201910, ccomisiones_otras := NA]
dataset[foto_mes == 201910, mcomisiones_otras := NA]
dataset[foto_mes == 201910, chomebanking_transacciones := NA]
dataset[foto_mes == 201910, ccomisiones_otras := NA]

# periodo 201904
dataset[foto_mes == 201904, ctarjeta_visa_debitos_automaticos := NA]
dataset[foto_mes == 201904, mttarjeta_visa_debitos_automaticos := NA]

# periodo 201907
dataset[foto_mes == 201907, Master_fultimo_cierre := NA]

# periodo 202009
dataset[foto_mes == 202009, Master_fultimo_cierre := NA]

# periodo 202106
dataset[foto_mes == 201904, Master_fultimo_cierre := NA]

dataset[foto_mes %in% PARAM$input$training, train := 1L]

# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# se trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
columnas_excluir <- c(
  "clase_ternaria", 
  "clase01",
  "foto_ano",
  "foto_mes"
)
campos_buenos <- setdiff(colnames(dataset), columnas_excluir)

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]


#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
# setwd(paste0("./exp/", PARAM$experimento, "/"))



# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


# genero el modelo
# param_completo <- c(PARAM$finalmodel$lgb_basicos,
#                     PARAM$finalmodel$optim)

# obtengo dataset de test
dapply <- dataset[foto_mes == PARAM$input$future]
ganancia_final <- data.table()
i <- 0

for(ksemilla in PARAM$input$semillas){
  # Establezco el Working Directory DEL EXPERIMENTO
  setwd(paste0("~/buckets/b1/exp/", PARAM$experimento))
  
  
  PARAM$finalmodel$lgb_basicos$seed = as.integer(ksemilla)
  i = i + 1
  
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)
  
  cat("\nEntrenando semilla: ",ksemilla,"\n")
  cat("Iteracion: ",i,"\n")
  
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )  
  
  cat("\nCreando directorio.")
  
  # Establezco el Working Directory DEL EXPERIMENTO
  dir.create(paste0(ksemilla,"/"), showWarnings = FALSE)
  setwd(paste0(ksemilla))
  
  #--------------------------------------
  # ahora imprimo la importancia de variables
  cat("\nImportancia de variables.\n")
  
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <- "impo.txt"
  
  fwrite(tb_importancia,
         file = archivo_importancia,
         sep = "\t"
  )
  
  #--------------------------------------
  cat("\nPrediciendo.\n")
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes, clase_ternaria)]
  tb_entrega[, prob := prediccion]
  
  # grabo las probabilidad del modelo
  
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)
  
  fwrite(subset(tb_entrega, select = -clase_ternaria),
         file = "prediccion.txt",
         sep = "\t"
  )
  
  # genero archivos con los  "envios" mejores
  # deben subirse "inteligentemente" a Kaggle para no malgastar submits
  # si la palabra inteligentemente no le significa nada aun
  # suba TODOS los archivos a Kaggle
  # espera a la siguiente clase sincronica en donde el tema sera explicado
  cat("\nCalculando ganancia y envios.\n")
  
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
      semilla = ksemilla,
      envios = envios,
      ganancia = PARAM$NEG_ganancia*nrow(tb_entrega[Predicted==1 & clase_ternaria!="BAJA+2"]) + PARAM$POS_ganancia*nrow(tb_entrega[Predicted==1 & clase_ternaria=="BAJA+2"])
    )
    
    #acumulamos las ganancias de todos los envios en un solo data table
    ganancia_semilla <- rbind(ganancia_semilla, ganancia_envios)
  }
  
  #acumulamos las ganancias de todos los envios, y todas las semillas. 
  ganancia_final <- rbindlist(list(ganancia_final, ganancia_semilla))
}

# guardamos las ganancias de las curvas entrenadas para las distintas semillas
setwd(paste0("~/buckets/b1/exp/", PARAM$experimento))
fwrite(ganancia_final,
       file = paste0("ganancias.csv"),
       sep = ","
)

cat("\n\nLa generacion de envios y probabilidades para cada semilla ha finalizado\n")
