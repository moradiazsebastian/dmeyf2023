# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")
require("dplyr")
require("rlang")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"
set.seed(42)
PARAM$input$semillas <- sample(10000:100000, 20, replace = FALSE)

# meses donde se entrena el modelo
# PARAM$input$validation <- c(202106)
PARAM$input$training <- c(201902, 201903, 201904, 201905, 201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202002, 202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105, 202106, 202107)
PARAM$input$future <- c(202109) # meses donde se aplica el modelo

#evaluacion estrategia
# PARAM$input$training <- c(201902, 201903, 201904, 201905, 201906, 201907, 201908, 201909, 201910, 201911, 201912, 202001, 202002, 202003, 202004, 202005, 202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105,202106)
# PARAM$input$future <- c(202107) # meses donde se aplica el modelo

# PARAM$finalmodel$semilla <- 288913
PARAM$experimento <- "semillero_competencia_baseline_lags"

# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
# max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
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
PARAM$finalmodel$optim$num_iterations <- 286
PARAM$finalmodel$optim$learning_rate <- 0.124337427260838
PARAM$finalmodel$optim$feature_fraction <- 0.822492837846036
PARAM$finalmodel$optim$min_data_in_leaf <- 27686
PARAM$finalmodel$optim$num_leaves <- 113
PARAM$finalmodel$optim$max_depth <- 18
PARAM$finalmodel$optim$bagging_freq <- 13
PARAM$finalmodel$optim$neg_bagging_fraction <- 0.822220085374548
PARAM$finalmodel$optim$pos_bagging_fraction <- 0.313468816900595

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por ahora, no hago nada


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html

# calculo de lags
calcular_lags <- function(dt, columnas, cantidad) {
  for(columna in columnas){
    i <- 0
    while(i<cantidad){
      i <- i+1
      
      lag <- paste0(columna,"_lag_", i)
      delta <- paste0(columna,"_lag_delta_", i,"\n")
      
      cat("\nProcesando columna:",columna)
      
      scolumna <- sym(columna)
      slag <- sym(lag)
      sdelta <- sym(delta)
      
      #generacion de lags y lags delta
      dt[, (lag) := shift(eval(scolumna),  n=i, fill=NA, type="lag")]
      dt[, (delta) := eval(scolumna)-eval(slag)]
    }
  }
  return(dt)
}

# feature engineering historico
# columnas objetivo
target = c("ctrx_quarter", "mpasivos_margen", "mautoservicio" , "ctarjeta_debito_transacciones" , "mtransferencias_emitidas" , "ctransferencias_emitidas" , "mpayroll" , "cpayroll_trx", "mcomisiones_mantenimiento", "mcomisiones_otras", "mcomisiones", "mcaja_ahorro", "mcuentas_saldo", "mtarjeta_visa_consumo","mprestamos_personales")

# calculando lags
dataset <- calcular_lags(dataset, target, 12)


#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
columnas_excluir <- c(
  "clase_ternaria", 
  "clase01", 
  "foto_mes"#,
#  "Master_Fvencimiento",
#  "Master_Finiciomora",
#  "Master_fultimo_cierre",
#  "Master_fechaalta",
#  "Visa_Fvencimiento",
# "Visa_Finiciomora",
# "Visa_fultimo_cierre",
#  "Visa_fechaalta"
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
  
  # Establezco el Working Directory DEL EXPERIMENTO
  dir.create(paste0(ksemilla,"/"), showWarnings = FALSE)
  setwd(paste0(ksemilla))
  
  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <- "impo.txt"
  
  fwrite(tb_importancia,
         file = archivo_importancia,
         sep = "\t"
  )
  
  #--------------------------------------
  
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
