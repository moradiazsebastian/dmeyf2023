pasos para reproducir segunda entrega de kaggle. 
1) armar clase ternaria usando el script de sql clase_ternaria.sql
2) ejecutar script de feature engineering FEH_para_competencia_02_en_SQL.ipynb. Arme lags para distintas variables y saque variables de fechas. Exclui los meses (201905,201910,202006) con base a graficas obtenidas de z505_graficar_zero_rate.r
3) ejecutar el script z823_lightgbm_binaria_BO para la optimizacion bayesiana y busqueda de hiperparametros. hice 25 iteraciones.  
4) ejecutar el script z824_lightgbm_final.r. Agregue todos los meses para que entrene, y exclui la variable foto_mes. el archivo con mejor score es KA8240_12000.csv (12k envios)