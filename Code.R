install.packages("dplyr")

library(forecast)
library(tsintermittent)
library(dplyr)


all_data <- read.csv("C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/AllData.csv", header = TRUE)


#Croston method for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.15, init = "naive" , outplot = 30)
bip1271crost
bip1271$cros_smoothed <- bip1271crost$frc.in

#Croston method for item BIP3819#
bip3819 <- select(all_data, X, BIP003819)
bip3819crost <- crost(bip3819[2], h = 5, w = 0.15, outplot = 1)
bip3819crost
bip3819$cros_smoothed <- bip3819crost$frc.in

#Croston method for item BIP1998#
bip1998 <- select(all_data, X, BIP001998)
bip1998crost <- crost(bip1998[2], h = 5, w = 0.15, outplot =1)
bip1998crost
bip1998$cros_smooth <- bip1998crost$frc.in

