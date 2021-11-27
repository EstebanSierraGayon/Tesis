install.packages("dplyr")

library(forecast)
library(tsintermittent)
library(dplyr)

?crost

all_data <- read.csv("C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/AllData.csv", header = TRUE)


#Croston method, SBA, SBJ for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.15, init = "naive" , outplot = 1)
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271

type_error <- cbind(croston_1271, SBA_1271, SBJ_1271)
dimensions <- dim(f.all)[2]
smooth_data <- t(tcrossprod(rep(1,k),bip1271[,2]))
errors <- smooth_data - type_error
errors[is.na(errors)] <- 0
ME <- apply(E, 2, mean)
MAE <- apply(abs(E), 2, mean)
RMSE <- sqrt(apply(E^2, 2, mean))
total_errors <- rbind(ME, MAE, RMSE)
print(total_errors)

plot(bip1271$BIP001271, type = 'b')
lines(ts(croston_1271, col='red'), frequency = 52), col='red')
lines(bip1271crost$frc.out, start=c(3, 37))

??plot






#Croston method for item BIP3819#
bip3819 <- select(all_data, X, BIP003819)
bip3819crost <- crost(bip3819[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip3819crost
bip3819$cros_smoothed <- bip3819crost$frc.in
bip3819SBA <- crost(bip3819[2], h = 5, type = 'sba',  init = 'naive', outplot = 1)
bip3819SBA
bip3819$SBA_smoothed <- bip3819SBA$frc.in
bip3819SBJ <- crost(bip3819[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip3819SBJ
bip3819$SBJ_smoothed <- bip3819SBJ$frc.in

#Croston method for item BIP1998#
bip1998 <- select(all_data, X, BIP001998)
bip1998crost <- crost(bip1998[2], h = 5, w = 0.15,  init = 'naive', outplot =1)
bip1998crost
bip1998$cros_smooth <- bip1998crost$frc.in
bip1998SBA <- crost(bip1998[2], h = 5, type = 'sba',  init = 'naive', outplot = 1)
bip1998SBA
bip1999$SBA_smoothed <- bip1998SBA$frc.in
bip1998SBJ <- crost(bip1998[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip1998SBJ
bip1998$SBJ_smoothed <- bip1998SBJ$frc.in

#Croston method for item BIP2015#
bip2015 <- select(all_data, X, BIP002015)
bip2015crost <- crost(bip2015[2], h = 5, w = 0.15,  init = 'naive', outplot = 1)
bip2015crost
bip2015$cros_smooth <- bip2015crost$frc.in
bip2015SBA <- crost(bip2015[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2015SBA
bip2015$SBA_smoothed <- bip2015SBA$frc.in
bip2015SBJ <- crost(bip2015[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip2015SBJ
bip2015$SBJ_smoothed <- bip2015SBJ$frc.in

#Croston method for item BIP2010
bip2010 <- select(all_data, X, BIP002010)
bip2010crost <- crost(bip2010[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip2010crost
bip2010$cros_smooth <- bip2010crost$frc.in
bip2010SBA <- crost(bip2010[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2010SBA
bip2010$SBA_smoothed <- bip2010SBA$frc.in
bip2010SBJ <- crost(bip2010[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2010SBJ
bip2010$SBJ_smoothed <- bip2010SBJ$frc.in

#Croston method for item BIP2898#
bip2898 <- select(all_data, X, BIP002898)
bip2898crost <- crost(bip2898[2], h = 5, w = 0.15,  init = 'naive', outplot = 1)
bip2898crost
bip2898$cros_smoothed <- bip2898crost$frc.in
bip2898SBA <- crost(bip2898[2], h = 5, type = 'sba',  init = 'naive', outplot = 1)
bip2898SBA
bip2898$SBA_smoothed <- bip2898SBA$frc.in
bip2898SBJ <- crost(bip2898[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip2898SBJ
bip2898$SBJ_smoothed <- bip2898SBJ$frc.in

#Croston method for item BIP1266#
bip1266 <- select(all_data, X, BIP001266)
bip1266crost <- crost(bip1266[2], h = 5, w = 0.15,  init = 'naive', outplot = 1)
bip1266crost
bip1266$crost_smoothed <- bip1266crost$frc.in
bip1266SBA <- crost(bip1266[2], h = 5, type = 'sba',  init = 'naive', outplot = 1)
bip1266SBA
bip1266$SBA_smoothed <- bip1266SBA$frc.in
bip1266SBJ <- crost(bip1266[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip1266SBJ
bip1266$SBJ_smoothed <- bip1266SBJ$frc.in

#Croston method for item BIP7983#
bip7983 <- select(all_data, X, BIP007983)
bip7983crost <- crost(bip7983[2], h = 5, w = 0.15,  init = 'naive', outplot = 1)
bip7983crost
bip7983$crost_smoothed <- bip7983crost$frc.in
bip7983SBA <- crost(bip7983[2], h = 5, type = 'sba',  init = 'naive', outplot = 1)
bip7983SBA
bip7983$SBA_smoothed <- bip7983SBA$frc.in
bip7983SBJ <- crost(bip7983[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip7983SBJ
bip7983$SBJ_smoothed <- bip7983SBJ$frc.in

#Croston method for item BIP5889#
bip5889 <- select(all_data, X, BIP005889)
bip5889crost <- crost(bip5889[2], h = 5, w = 0.15,  init = 'naive', outplot = 1)
bip5889crost
bip5889$crost_smoothed <- bip5889crost$frc.in
bip5889SBA <- crost(bip5889[2], h = 5, type = 'sba',  init = 'naive',  outplot = 1)
bip5889SBA
bip5898$SBA_smoothed <- bip5889SBA$frc.in
bip5889SBJ <- crost(bip5889[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip5889SBJ
bip5889$SBJ_smoothed <- bip5889SBJ$frc.in

#Croston model for item BIP8645#
bip8645 <- select(all_data, X, BIP008645)
bip8645crost <- crost(bip8645[2], h = 5, w = 0.15,  init = 'naive', outplot = 1)
bip8645crost
bip8645$crost_smothed <- bip8645crost$frc.in
bip8645SBA <- crost(bip8645[2], h = 5, type = 'sba',  init = 'naive', outplot = 1)
bip8645SBA
bip8645$SBA_smoothed <- bip8645SBA$frc.in
bip8645SBJ <- crost(bip8645[2], h = 5, type = 'sbj',  init = 'naive', outplot = 1)
bip8645SBJ
bip8645$SBJ_smoothed <- bip8645SBJ$frc.in

#Croston model for item BIP5467#
bip5467 <- select(all_data, X, BIP005467)
bip5467crost <- crost(bip5467[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip5467crost
bip5467$crost_smoothed <- bip5467crost$frc.in
bip5467SBA <- crost(bip5467[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5467SBA
bip5467$SBA_smoothed <- bip5467SBA$frc.in
bip5467SBJ <- crost(bip5467[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5467SBJ
bip5467$SBJ_smoothed <- bip5467SBJ$frc.in

#Croston model for item BIP8920#
bip8920 <- select(all_data, X, BIP008920)
bip8920crost <- crost(bip8920[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip8920crost
bip8920$crost_smoothed <- bip8920crost$frc.in
bip8920SBA <- crost(bip8920[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip8920SBA
bip8920$SBA_smoothed <- bip8920SBA$frc.in
bip8920SBJ <- crost(bip8920[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip8920SBJ
bip8920$SBJ_smoothed <- bip8920SBJ$frc.in

#Croston model for item BIP9107#
bip9107 <- select(all_data, X, BIP009107)
bip9107crost <- crost(bip9107[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip9107crost
bip9107$crost_smoothed <- bip9107crost$frc.in
bip9107SBA <- crost(bip9107[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip9107SBA
bip9107$SBA_smoothed <- bip9107SBA$frc.in
bip9107SBJ <- crost(bip9107[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip9107SBJ
bip9107$SBJ_smoothed <- bip9107SBJ$frc.in

#Croston model for item BIP6806#
bip6806 <- select(all_data, X, BIP006806)
bip6806crost <- crost(bip6806[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip6806crost
bip6806$crost_smoothed <- bip6806crost$frc.in
bip6806SBA <- crost(bip6806[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip6806SBA
bip6806$SBA_smoothed <- bip6806SBA$frc.in
bip6806SBJ <- crost(bip6806[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip6806SBJ
bip6806$SBJ_smoothed <- bip6806SBJ$frc.in

#Croston model for item BIP5887#
bip5887 <- select(all_data, X, BIP005887)
bip5887crost <- crost(bip5887[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip5887crost
bip5887$crost_smoothed <- bip5887crost$frc.in
bip5887SBA <- crost(bip5887[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5887SBA
bip5887$SBA_smoothed <- bip5887SBA$frc.in
bip5887SBJ <- crost(bip5887[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5887SBJ
bip5887$SBJ_smoothed <- bip5887SBJ$frc.in

#Croston model for item BIP8013#
bip8013 <- select(all_data, X, BIP008013)
bip8013crost <- crost(bip8013[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip8013crost
bip8013$crost_smoothed <- bip8013crost$frc.in
bip8013SBA <- crost(bip8013[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip8013SBA
bip8013$SBA_smoothed <- bip8013SBA$frc.in
bip8013SBJ <- crost(bip8013[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip8013SBJ
bip8013$SBJ_smoothed <- bip8013SBJ$frc.in

#Croston model for item BIP3816#
bip3816 <- select(all_data, X, BIP003816)
bip3816crost <- crost(bip3816[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip3816crost
bip3816$crost_smoothed <- bip3816crost$frc.in
bip3816SBA <- crost(bip3816[2], h = 5, type = 'sba', init = 'naive',  outplot = 1)
bip3816SBA
bip3816$SBA_smoothed <- bip3816SBA$frc.in
bip3816SBJ <- crost(bip3816[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip3816SBJ
bip3816$SBJ_smoothed <- bip3816SBJ$frc.in

#Croston model for item BIP7269#
bip7269 <- select(all_data, X, BIP007269)
bip7269crost <- crost(bip7269[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip7269crost
bip7269$crost_smoothed <- bip7269crost$frc.in
bip7269SBA <- crost(bip7269[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip7269SBA
bip7269$SBA_smoothed <- bip7269SBA$frc.in
bip7269SBJ <- crost(bip7269[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip7269SBJ
bip7269$SBJ_smoothed <- bip7269SBJ$frc.in

#Croston model for item BIP2846#
bip2846 <- select(all_data, X, BIP002846)
bip2846crost <- crost(bip2846[2], h = 5, w = 0.15, init = 'naive', outplot = 1)
bip2846crost
bip2846$crost_smoothed <- bip2846crost$frc.in
bip2846SBA <- crost(bip2846[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2846SBA
bip2846$SBA_smoothed <- bip2846SBA$frc.in
bip2846SBJ <- crost(bip2846[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2846SBJ
bip2846$SBJ_smoothed <- bip2846SBJ$frc.in

#Croston model for item BIP7495#
bip7495 <- select(all_data, X, BIP007495)
bip7495crost <- crost(bip7495[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip7495crost
bip7495$crost_smoothed <- bip7495crost$frc.in
bip7495SBA <- crost(bip7495[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip7495SBA
bip7495$SBA_smoothed <- bip7495SBA$frc.in
bip7495SBJ <- crost(bip7495[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip7495SBJ
bip7495$SBJ_smoothed <- bip7495SBJ$frc.in

#Croston model for item BIP1478#
bip1478 <- select(all_data, X, BIP001478)
bip1478crost <- crost(bip1478[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip1478crost
bip1478$crost_smoothed <- bip1478crost$frc.in
bip1478SBA <- crost(bip1478[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip1478SBA
bip1478$SBA_smoothed <- bip1478SBA$frc.in
bip1478SBJ <- crost(bip1478[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip1478SBJ
bip1478$SBJ_smoothed <- bip1478SBJ$frc.in

#Croston model for item BIP1479#
bip1479 <- select(all_data, X, BIP001479)
bip1479crost <- crost(bip1479[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip1479crost
bip1479$crost_smoothed <- bip1479crost$frc.in
bip1479SBA <- crost(bip1479[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip1479SBA
bip1479$SBA_smoothed <- bip1479SBA$frc.in
bip1479SBJ <- crost(bip1479[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip1479SBJ
bip1479$SBJ_smoothed <- bip1479SBJ$frc.in

#Croston model for item BIP4338#
bip4338 <- select(all_data, X, BIP004338)
bip4338crost <- crost(bip4338[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip4338crost
bip4338$crost_smoothed <- bip4338crost$frc.in
bip4338SBA <- crost(bip4338[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip4338SBA
bip4338$SBA_smoothed <- bip4338SBA$frc.in
bip4338SBJ <- crost(bip4338[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip4338SBJ
bip4338$SBJ_smoothed <- bip4338SBJ$frc.in

#Croston model for item BIP4394#
bip4394 <- select(all_data, X, BIP004394)
bip4394crost <- crost(bip4394[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip4394crostcrost
bip4394$crost_smoothed <- bip4394crost$frc.in
bip4394SBA <- crost(bip4394[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip4394SBA
bip4394$SBA_smoothed <- bip4394SBA$frc.in
bip4394SBJ <- crost(bip4394[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip4394SBJ
bip4394$SBJ_smoothed <- bip4394SBJ$frc.in

#Croston model for item BIP8641#
bip8641 <- select(all_data, X, BIP008641)
bip8641crost <- crost(bip8641[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip8641crost
bip8641$crost_smoothed <- bip8641crost$frc.in
bip8641SBA <- crost(bip8641[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip8641SBA
bip8641$SBA_smoothed <- bip8641SBA$frc.in
bip8641SBJ <- crost(bip8641[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip8641SBJ
bip8641$SBJ_smoothed <- bip8641SBJ$frc.in

#Croston model for item BIP2861#
bip2861 <- select(all_data, X, BIP002861)
bip2861crost <- crost(bip2861[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip2861crost
bip2861$crost_smoothed <- bip2861crost$frc.in
bip2861SBA <- crost(bip2861[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2861SBA
bip2861$SBA_smoothed <- bip2861SBA$frc.in
bip2861SBJ <- crost(bip2861[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2861SBJ
bip2861$SBJ_smoothed <- bip2861SBJ$frc.in

#Croston model for item BIP1480#
bip1480 <- select(all_data, X, BIP001480)
bip1480crost <- crost(bip1480[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip1480crost
bip1480$crost_smoothed <- bip1480crost$frc.in
bip1480SBA <- crost(bip1480[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip1480SBA
bip1480$SBA_smoothed <- bip1480SBA$frc.in
bip1480SBJ <- crost(bip1480[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip1480SBJ
bip1480$SBJ_smoothed <- bip1480SBJ$frc.in

#Croston model for item BIP2201#
bip2201 <- select(all_data, X, BIP002201)
bip2201crost <- crost(bip2201[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip2201crost
bip2201$crost_smoothed <- bip2201crost$frc.in
bip2201SBA <- crost(bip2201[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2201SBA
bip2201$SBA_smoothed <- bip2201SBA$frc.in
bip2201SBJ <- crost(bip2201[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2201SBJ
bip2201$SBJ_smoothed <- bip2201SBJ$frc.in

#Croston model for item BIP5677#
bip5677 <- select(all_data, X, BIP005677)
bip5677crost <- crost(bip5677[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip5677crost
bip5677$crost_smoothed <- bip5677crost$frc.in
bip5677SBA <- crost(bip5677[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5677SBA
bip5677$SBA_smoothed <- bip5677SBA$frc.in
bip5677SBJ <- crost(bip5677[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5677SBJ
bip5677$SBJ_smoothed <- bip5677SBJ$frc.in

#Croston model for item BIP2459#
bip2459 <- select(all_data, X, BIP002459)
bip2459crost <- crost(bip2459[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip2459crost
bip2459$crost_smoothed <- bip2459crost$frc.in
bip2459SBA <- crost(bip2459[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2459SBA
bip2459$SBA_smoothed <- bip2459SBA$frc.in
bip2459SBJ <- crost(bip2459[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2459SBJ
bip2459$SBJ_smoothed <- bip2459SBJ$frc.in

#Croston model for item BIP1268#
bip1268 <- select(all_data, X, BIP001268)
bip1268crost <- crost(bip1268[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip1268crost
bip1268$crost_smoothed <- bip1268crost$frc.in
bip1268SBA <- crost(bip1268[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip1268SBA
bip1268$SBA_smoothed <- bip1268SBA$frc.in
bip1268SBJ <- crost(bip1268[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip1268SBJ
bip1268$SBJ_smoothed <- bip1268SBJ$frc.in

#Croston model for item BIP7493#
bip7493 <- select(all_data, X, BIP007493)
bip7493crost <- crost(bip7493[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip7493crost
bip7493$crost_smoothed <- bip7493crost$frc.in
bip7493SBA <- crost(bip7493[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip7493SBA
bip7493$SBA_smoothed <- bip7493SBA$frc.in
bip7493SBJ <- crost(bip7493[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip7493SBJ
bip7493$SBJ_smoothed <- bip7493SBJ$frc.in

#Croston model for item BIP8649#
bip8649 <- select(all_data, X, BIP008649)
bip8649crost <- crost(bip8649[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip8649crost
bip8649$crost_smoothed <- bip8649crost$frc.in
bip8649SBA <- crost(bip8649[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip8649SBA
bip8649$SBA_smoothed <- bip8649SBA$frc.in
bip8649SBJ <- crost(bip8649[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip8649SBJ
bip8649$SBJ_smoothed <- bip8649SBJ$frc.in

#Croston model for item BIP5601#
bip5601 <- select(all_data, X, BIP005601)
bip5601crost <- crost(bip5601[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip5601crost
bip5601$crost_smoothed <- bip5601crost$frc.in
bip5601SBA <- crost(bip5601[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5601SBA
bip5601$SBA_smoothed <- bip5601SBA$frc.in
bip5601SBJ <- crost(bip5601[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5601SBJ
bip5601$SBJ_smoothed <- bip5601SBJ$frc.in

#Croston model for item BIP3668#
bip3668 <- select(all_data, X, BIP003668)
bip3668crost <- crost(bip3668[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip3668crost
bip3668$crost_smoothed <- bip3668crost$frc.in
bip3668SBA <- crost(bip3668[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip3668SBA
bip3668$SBA_smoothed <- bip3668SBA$frc.in
bip3668SBJ <- crost(bip3668[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip3668SBJ
bip3668$SBJ_smoothed <- bip3668SBJ$frc.in

#Croston model for item BIP3217#
bip3217 <- select(all_data, X, BIP003217)
bip3217crost <- crost(bip3217[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip3217crost
bip3217$crost_smoothed <- bip3217crost$frc.in
bip3217SBA <- crost(bip3217[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip3217SBA
bip3217$SBA_smoothed <- bip3217SBA$frc.in
bip3217SBJ <- crost(bip3217[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip3217SBJ
bip3217$SBJ_smoothed <- bip3217SBJ$frc.in

#Croston model for item BIP5556#
bip5556 <- select(all_data, X, BIP005556)
bip5556crost <- crost(bip5556[2], h = 5, w = 0.15, type = 'naive',outplot = 1)
bip5556crost
bip5556$crost_smoothed <- bip5556crost$frc.in
bip5556SBA <- crost(bip5556[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5556SBA
bip5556$SBA_smoothed <- bip5556SBA$frc.in
bip5556SBJ <- crost(bip5556[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5556SBJ
bip5556$SBJ_smoothed <- bip5556SBJ$frc.in

#Croston model for item BIP7666#
bip7666 <- select(all_data, X, BIP007666)
bip7666crost <- crost(bip7666[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip7666crost
bip7666$crost_smoothed <- bip7666crost$frc.in
bip7666SBA <- crost(bip7666[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip7666SBA
bip7666$SBA_smoothed <- bip7666SBA$frc.in
bip7666SBJ <- crost(bip7666[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip7666SBJ
bip7666$SBJ_smoothed <- bip7666SBJ$frc.in

#Croston model for item BIP3105#
bip3105 <- select(all_data, X, BIP003105)
bip3105crost <- crost(bip3105[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip3105crost
bip3105$crost_smoothed <- bip3105crost$frc.in
bip3105SBA <- crost(bip3105[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip3105SBA
bip3105$SBA_smoothed <- bip3105SBA$frc.in
bip3105SBJ <- crost(bip3105[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip3105SBJ
bip3105$SBJ_smoothed <- bip3105SBJ$frc.in

#Croston model for item BIP4395#
bip4395 <- select(all_data, X, BIP004395)
bip4395crost <- crost(bip4395[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip4395crost
bip4395$crost_smoothed <- bip4395crost$frc.in
bip4395SBA <- crost(bip4395[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip4395SBA
bip4395$SBA_smoothed <- bip4395SBA$frc.in
bip4395SBJ <- crost(bip4395[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip4395SBJ
bip4395$SBJ_smoothed <- bip4395SBJ$frc.in

#Croston model for item BIP5900#
bip5900 <- select(all_data, X, BIP005900)
bip5900crost <- crost(bip5900[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip5900crost
bip5900$crost_smoothed <- bip5900crost$frc.in
bip5900SBA <- crost(bip5900[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5900SBA
bip5900$SBA_smoothed <- bip5900SBA$frc.in
bip5900SBJ <- crost(bip5900[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5900SBJ
bip5900$SBJ_smoothed <- bip5900SBJ$frc.in

#Croston model for item BIP8822#
bip8822 <- select(all_data, X, BIP008822)
bip8822crost <- crost(bip8822[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip8822crost
bip8822$crost_smoothed <- bip8822crost$frc.in
bip8822SBA <- crost(bip8822[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip8822SBA
bip8822$SBA_smoothed <- bip8822SBA$frc.in
bip8822SBJ <- crost(bip8822[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip8822SBJ
bip8822$SBJ_smoothed <- bip8822SBJ$frc.in

#Croston model for item BIP4396#
bip4396 <- select(all_data, X, BIP004396)
bip4396crost <- crost(bip4396[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip4396crost
bip4396$crost_smoothed <- bip4396crost$frc.in
bip4396SBA <- crost(bip4396[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip4396SBA
bip4396$SBA_smoothed <- bip4396SBA$frc.in
bip4396SBJ <- crost(bip4396[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip4396SBJ
bip4396$SBJ_smoothed <- bip4396SBJ$frc.in

#Croston model for item BIP9106#
bip9106 <- select(all_data, X, BIP009106)
bip9106crost <- crost(bip9106[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip9106crost
bip9106$crost_smoothed <- bip9106crost$frc.in
bip9106SBA <- crost(bip2459[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip9106SBA
bip9106$SBA_smoothed <- bip9106SBA$frc.in
bip9106SBJ <- crost(bip9106[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip9106SBJ
bip9106$SBJ_smoothed <- bip9106SBJ$frc.in

#Croston model for item BIP5464#
bip5464 <- select(all_data, X, BIP005464)
bip5464crost <- crost(bip5464[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip5464crost
bip5464$crost_smoothed <- bip5464crost$frc.in
bip5464SBA <- crost(bip2459[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip5464SBA
bip5464$SBA_smoothed <- bip5464SBA$frc.in
bip5464SBJ <- crost(bip5464[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip5464SBJ
bip5464$SBJ_smoothed <- bip5464SBJ$frc.in

#Croston model for item BIP1877#
bip1877 <- select(all_data, X, BIP001877)
bip1877crost <- crost(bip1877[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip1877crost
bip1877$crost_smoothed <- bip1877crost$frc.in
bip1877SBA <- crost(bip1877[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip1877SBA
bip1877$SBA_smoothed <- bip1877SBA$frc.in
bip1877SBJ <- crost(bip1877[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip1877SBJ
bip1877$SBJ_smoothed <- bip1877SBJ$frc.in

#Croston model for item BIP2894#
bip2894 <- select(all_data, X, BIP002894)
bip2894crost <- crost(bip2894[2], h = 5, w = 0.15, type = 'naive', outplot = 1)
bip2894crost
bip2894$crost_smoothed <- bip2894crost$frc.in
bip2894SBA <- crost(bip2894[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2894SBA
bip2894$SBA_smoothed <- bip2894SBA$frc.in
bip2894SBJ <- crost(bip2894[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2894SBJ
bip2894$SBJ_smoothed <- bip2894SBJ$frc.in

#Croston model for item BIP2899#
bip2899 <- select(all_data, X, BIP002899)
bip2899crost <- crost(bip2899[2], h = 5, w = 0.15, type = 'naive',  outplot = 1)
bip2899crost
bip2899$crost_smoothed <- bip2899crost$frc.in
bip2899SBA <- crost(bip2899[2], h = 5, type = 'sba', init = 'naive', outplot = 1)
bip2899SBA
bip2899$SBA_smoothed <- bip2899SBA$frc.in
bip2899SBJ <- crost(bip2899[2], h = 5, type = 'sbj', init = 'naive', outplot = 1)
bip2899SBJ
bip2899$SBJ_smoothed <- bip2899SBJ$frc.in





