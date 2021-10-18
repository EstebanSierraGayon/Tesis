##prueba utilizando demanda intermitente (15 periodos) usando libreria tsintermittent
prueba5_1 <- read.csv("C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/prueba5.csv", header = TRUE)
prueba5_1_ts <- ts(prueba5_1, start = c(2017,12), end = c(2020,11), frequency = 12)
prueba5_1_ts
plot(prueba5_1_ts)

prueba5_1_ts_crost <- crost(prueba5_1_ts, h = 10, outplot = 1)$frc.out
print(prueba5_1_ts_crost)
