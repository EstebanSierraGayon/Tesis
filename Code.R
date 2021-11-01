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

#Croston method for item BIP2015#
bip2015 <- select(all_data, X, BIP002015)
bip2015crost <- crost(bip2015[2], h = 5, w = 0.15, outplot = 1)
bip2015crost
bip2015$cros_smooth <- bip2015crost$frc.in

#Croston method for item BIP2010
bip2010 <- select(all_data, X, BIP002010)
bip2010crost <- crost(bip2010[2], h = 5, w = 0.15, outplot = 1)
bip2010crost
bip2010$cros_smooth <- bip2010crost$frc.in

#Croston method for item BIP2898#
bip2898 <- select(all_data, X, BIP002898)
bip2898crost <- crost(bip2898[2], h = 5, outplot = 1)
bip2898crost
bip2898$cros_smoothed <- bip2898crost$frc.in

#Croston method for item BIP1266#
bip1266 <- select(all_data, X, BIP001266)
bip1266crost <- crost(bip1266[2], h = 5, outplot = 1)
bip1266crost
bip1266$crost_smoothed <- bip1266crost$frc.in

#Croston method for item BIP7983#
bip7983 <- select(all_data, X, BIP007983)
bip7983crost <- crost(bip7983[2], h = 5, outplot = 1)
bip7983crost
bip7983$crost_smoothed <- bip7983crost$frc.in

#Croston method for item BIP5889#
bip5889 <- select(all_data, X, BIP005889)
bip5889crost <- crost(bip5889[2], h = 5, outplot = 1)
bip5889crost
bip5889$crost_smoothed <- bip5889crost$frc.in

#Croston model for item BIP8645#
bip8645 <- select(all_data, X, BIP008645)
bip8645crost <- crost(bip8645[2], h = 5, outplot = 1)
bip8645crost
bip8645$crost_smothed <- bip8645crost$frc.in

#Croston model for item BIP5467#
bip5467 <- select(all_data, X, BIP005467)
bip5467crost <- crost(bip5467[2], h = 5, outplot = 1)
bip5467crost
bip5467$crost_smoothed <- bip5467crost$frc.in

#Croston model for item BIP8920#
bip8920 <- select(all_data, X, BIP008920)
bip8920crost <- crost(bip8920[2], h = 5, outplot = 1)
bip8920crost
bip8920$crost_smoothed <- bip8920crost$frc.in

#Croston model for item BIP9107#
bip9107 <- select(all_data, X, BIP009107)
bip9107crost <- crost(bip9107[2], h = 5, outplot = 1)
bip9107crost
bip9107$crost_smoothed <- bip9107crost$frc.in

#Croston model for item BIP6806#
bip6806 <- select(all_data, X, BIP006806)
bip6806crost <- crost(bip6806[2], h = 5, outplot = 1)
bip6806crost
bip6806$crost_smoothed <- bip6806crost$frc.in

#Croston model for item BIP5887#
bip5887 <- select(all_data, X, BIP005887)
bip5887crost <- crost(bip5887[2], h = 5, outplot = 1)
bip5887crost
bip5887$crost_smoothed <- bip5887crost$frc.in

#Croston model for item BIP8013#
bip8013 <- select(all_data, X, BIP008013)
bip8013crost <- crost(bip8013[2], h = 5, outplot = 1)
bip8013crost
bip8013$crost_smoothed <- bip8013crost$frc.in

#Croston model for item BIP3816#
bip3816 <- select(all_data, X, BIP003816)
bip3816crost <- crost(bip3816[2], h = 5, outplot = 1)
bip3816crost
bip3816$crost_smoothed <- bip3816crost$frc.in

#Croston model for item BIP7269#
bip7269 <- select(all_data, X, BIP007269)
bip7269crost <- crost(bip7269[2], h = 5, outplot = 1)
bip7269crost
bip7269$crost_smoothed <- bip7269crost$frc.in

#Croston model for item BIP2846#
bip2846 <- select(all_data, X, BIP002846)
bip2846crost <- crost(bip2846[2], h = 5, outplot = 1)
bip2846crost
bip2846$crost_smoothed <- bip2846crost$frc.in

#Croston model for item BIP7495#
bip7495 <- select(all_data, X, BIP007495)
bip7495crost <- crost(bip7495[2], h = 5, outplot = 1)
bip7495crost
bip7495$crost_smoothed <- bip7495crost$frc.in

#Croston model for item BIP1478#
bip1478 <- select(all_data, X, BIP001478)
bip1478crost <- crost(bip1478[2], h = 5, outplot = 1)
bip1478crost
bip1478$crost_smoothed <- bip1478crost$frc.in

#Croston model for item BIP1479#
bip1479 <- select(all_data, X, BIP001479)
bip1479crost <- crost(bip1479[2], h = 5, outplot = 1)
bip1479crost
bip1479$crost_smoothed <- bip1479crost$frc.in

#Croston model for item BIP4338#
bip4338 <- select(all_data, X, BIP004338)
bip4338crost <- crost(bip4338[2], h = 5, outplot = 1)
bip4338crost
bip4338$crost_smoothed <- bip4338crost$frc.in

#Croston model for item BIP4394#
bip4394 <- select(all_data, X, BIP004394)
bip4394crost <- crost(bip4394[2], h = 5, outplot = 1)
bip4394crostcrost
bip4394$crost_smoothed <- bip4394crost$frc.in

#Croston model for item BIP8641#
bip8641 <- select(all_data, X, BIP008641)
bip8641crost <- crost(bip8641[2], h = 5, outplot = 1)
bip8641crost
bip8641$crost_smoothed <- bip8641crost$frc.in

#Croston model for item BIP2861#
bip2861 <- select(all_data, X, BIP002861)
bip2861crost <- crost(bip2861[2], h = 5, outplot = 1)
bip2861crost
bip2861$crost_smoothed <- bip2861crost$frc.in

#Croston model for item BIP1480#
bip1480 <- select(all_data, X, BIP001480)
bip1480crost <- crost(bip1480[2], h = 5, outplot = 1)
bip1480crost
bip1480$crost_smoothed <- bip1480crost$frc.in

#Croston model for item BIP2201#
bip2201 <- select(all_data, X, BIP002201)
bip2201crost <- crost(bip2201[2], h = 5, outplot = 1)
bip2201crost
bip2201$crost_smoothed <- bip2201crost$frc.in












