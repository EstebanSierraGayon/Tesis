install.packages("dplyr")

library(forecast)
library(tsintermittent)
library(dplyr)

?crost

all_data <- read.csv("D:/Esteban/TESIS/Tesis/Tesis/AllData.csv", header = TRUE)


#Croston method, SBA, SBJ for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.15, init = "naive")
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, type = 'sba', init = 'naive')
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, type = 'sbj', init = 'naive')
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271

type_error_1271 <- cbind(croston_1271, SBA_1271, SBJ_1271)
dimensions_1271 <- dim(type_error_1271)[2]
smooth_data_1271 <- t(tcrossprod(rep(1,dimensions_1271),bip1271[,2]))
errors_1271 <- smooth_data_1271 - type_error_1271
errors_1271[is.na(errors_1271)] <- 0
ME_1271 <- apply(errors_1271, 2, mean)
MAE_1271 <- apply(abs(errors_1271), 2, mean)
RMSE_1271 <- sqrt(apply(errors_1271^2, 2, mean))
tot_err_1271 <- rbind(ME_1271, MAE_1271, RMSE_1271)
print(tot_err_1271)

plot(ts(bip1271$BIP001271, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1271, frequency = 52), col='red')
lines(ts(bip1271crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1271, frequency = 52), col='blue')
lines(ts(bip1271SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1271, frequency = 52), col='orange')
lines(ts(bip1271SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP3819#
bip3819 <- select(all_data, X, BIP003819)
bip3819crost <- crost(bip3819[2], h = 5, w = 0.15, init = "naive")
bip3819crost
croston_3819 <- bip3819crost$frc.in
bip3819$cros_smoothed <- croston_3819
bip3819SBA <- crost(bip3819[2], h = 5, type = 'sba', init = 'naive')
bip3819SBA
SBA_3819 <- bip3819SBA$frc.in
bip3819$SBA_smoothed <- SBA_3819
bip3819SBJ <- crost(bip3819[2], h = 5, type = 'sbj', init = 'naive')
bip3819SBJ
SBJ_3819 <- bip3819SBJ$frc.in
bip3819$SBJ_smoothed <- SBJ_3819

type_error_3819 <- cbind(croston_3819, SBA_3819, SBJ_3819)
dimensions_3819 <- dim(type_error_3819)[2]
smooth_data_3819 <- t(tcrossprod(rep(1,dimensions_3819),bip3819[,2]))
errors_3819 <- smooth_data_3819 - type_error_3819
errors_3819[is.na(errors_3819)] <- 0
ME_3819 <- apply(errors_3819, 2, mean)
MAE_3819 <- apply(abs(errors_3819), 2, mean)
RMSE_3819 <- sqrt(apply(errors_3819^2, 2, mean))
tot_err_3819 <- rbind(ME_3819, MAE_3819, RMSE_3819)
print(tot_err_3819)

plot(ts(bip3819$BIP003819, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_3819, frequency = 52), col='red')
lines(ts(bip3819crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_3819, frequency = 52), col='blue')
lines(ts(bip3819SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_3819, frequency = 52), col='orange')
lines(ts(bip3819SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP1998#
bip1998 <- select(all_data, X, BIP001998)
bip1998crost <- crost(bip1998[2], h = 5, w = 0.15, init = "naive")
bip1998crost
croston_1998 <- bip1998crost$frc.in
bip1998$cros_smoothed <- croston_1998
bip1998SBA <- crost(bip1998[2], h = 5, type = 'sba', init = 'naive')
bip1998SBA
SBA_1998 <- bip1998SBA$frc.in
bip1998$SBA_smoothed <- SBA_1998
bip1998SBJ <- crost(bip1998[2], h = 5, type = 'sbj', init = 'naive')
bip1998SBJ
SBJ_1998 <- bip1998SBJ$frc.in
bip1998$SBJ_smoothed <- SBJ_1998

type_error_1998 <- cbind(croston_1998, SBA_1998, SBJ_1998)
dimensions_1998 <- dim(type_error_1998)[2]
smooth_data_1998 <- t(tcrossprod(rep(1,dimensions_1998),bip1998[,2]))
errors_1998 <- smooth_data_1998 - type_error_1998
errors_1998[is.na(errors_1998)] <- 0
ME_1998 <- apply(errors_1998, 2, mean)
MAE_1998 <- apply(abs(errors_1998), 2, mean)
RMSE_1998 <- sqrt(apply(errors_1998^2, 2, mean))
tot_err_1998 <- rbind(ME_1998, MAE_1998, RMSE_1998)
print(tot_err_1998)

plot(ts(bip1998$BIP001998, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1998, frequency = 52), col='red')
lines(ts(bip1998crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1998, frequency = 52), col='blue')
lines(ts(bip1998SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1998, frequency = 52), col='orange')
lines(ts(bip1998SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP2015#
bip2015 <- select(all_data, X, BIP002015)
bip2015crost <- crost(bip2015[2], h = 5, w = 0.15, init = "naive")
bip2015crost
croston_2015 <- bip2015crost$frc.in
bip2015$cros_smoothed <- croston_2015
bip2015SBA <- crost(bip2015[2], h = 5, type = 'sba', init = 'naive')
bip2015SBA
SBA_2015 <- bip2015SBA$frc.in
bip2015$SBA_smoothed <- SBA_2015
bip2015SBJ <- crost(bip2015[2], h = 5, type = 'sbj', init = 'naive')
bip2015SBJ
SBJ_2015 <- bip2015SBJ$frc.in
bip2015$SBJ_smoothed <- SBJ_2015

type_error_2015 <- cbind(croston_2015, SBA_2015, SBJ_2015)
dimensions_2015 <- dim(type_error_2015)[2]
smooth_data_2015 <- t(tcrossprod(rep(1,dimensions_2015),bip2015[,2]))
errors_2015 <- smooth_data_2015 - type_error_2015
errors_2015[is.na(errors_2015)] <- 0
ME_2015 <- apply(errors_2015, 2, mean)
MAE_2015 <- apply(abs(errors_2015), 2, mean)
RMSE_2015 <- sqrt(apply(errors_2015^2, 2, mean))
tot_err_2015 <- rbind(ME_2015, MAE_2015, RMSE_2015)
print(tot_err_2015)

plot(ts(bip2015$BIP002015, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2015, frequency = 52), col='red')
lines(ts(bip2015crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2015, frequency = 52), col='blue')
lines(ts(bip2015SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2015, frequency = 52), col='orange')
lines(ts(bip2015SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP2010
bip2010 <- select(all_data, X, BIP002010)
bip2010crost <- crost(bip2010[2], h = 5, w = 0.15, init = "naive")
bip2010crost
croston_2010 <- bip2010crost$frc.in
bip2010$cros_smoothed <- croston_2010
bip2010SBA <- crost(bip2010[2], h = 5, type = 'sba', init = 'naive')
bip2010SBA
SBA_2010 <- bip2010SBA$frc.in
bip2010$SBA_smoothed <- SBA_2010
bip2010SBJ <- crost(bip2010[2], h = 5, type = 'sbj', init = 'naive')
bip2010SBJ
SBJ_2010 <- bip2010SBJ$frc.in
bip2010$SBJ_smoothed <- SBJ_2010

type_error_2010 <- cbind(croston_2010, SBA_2010, SBJ_2010)
dimensions_2010 <- dim(type_error_2010)[2]
smooth_data_2010 <- t(tcrossprod(rep(1,dimensions_2010),bip2010[,2]))
errors_2010 <- smooth_data_2010 - type_error_2010
errors_2010[is.na(errors_2010)] <- 0
ME_2010 <- apply(errors_2010, 2, mean)
MAE_2010 <- apply(abs(errors_2010), 2, mean)
RMSE_2010 <- sqrt(apply(errors_2010^2, 2, mean))
tot_err_2010 <- rbind(ME_2010, MAE_2010, RMSE_2010)
print(tot_err_2010)

plot(ts(bip2010$BIP002010, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2010, frequency = 52), col='red')
lines(ts(bip2010crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2010, frequency = 52), col='blue')
lines(ts(bip2010SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2010, frequency = 52), col='orange')
lines(ts(bip2010SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP2898#
bip2898 <- select(all_data, X, BIP002898)
bip2898crost <- crost(bip2898[2], h = 5, w = 0.15, init = "naive")
bip2898crost
croston_2898 <- bip2898crost$frc.in
bip2898$cros_smoothed <- croston_2898
bip2898SBA <- crost(bip2898[2], h = 5, type = 'sba', init = 'naive')
bip2898SBA
SBA_2898 <- bip2898SBA$frc.in
bip2898$SBA_smoothed <- SBA_2898
bip2898SBJ <- crost(bip2898[2], h = 5, type = 'sbj', init = 'naive')
bip2898SBJ
SBJ_2898 <- bip2898SBJ$frc.in
bip2898$SBJ_smoothed <- SBJ_2898

type_error_2898 <- cbind(croston_2898, SBA_2898, SBJ_2898)
dimensions_2898 <- dim(type_error_2898)[2]
smooth_data_2898 <- t(tcrossprod(rep(1,dimensions_2898),bip2898[,2]))
errors_2898 <- smooth_data_2898 - type_error_2898
errors_2898[is.na(errors_2898)] <- 0
ME_2898 <- apply(errors_2898, 2, mean)
MAE_2898 <- apply(abs(errors_2898), 2, mean)
RMSE_2898 <- sqrt(apply(errors_2898^2, 2, mean))
tot_err_2898 <- rbind(ME_2898, MAE_2898, RMSE_2898)
print(tot_err_2898)

plot(ts(bip2898$BIP002898, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2898, frequency = 52), col='red')
lines(ts(bip2898crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2898, frequency = 52), col='blue')
lines(ts(bip2898SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2898, frequency = 52), col='orange')
lines(ts(bip2898SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP1266#
bip1266 <- select(all_data, X, BIP001266)
bip1266crost <- crost(bip1266[2], h = 5, w = 0.15, init = "naive")
bip1266crost
croston_1266 <- bip1266crost$frc.in
bip1266$cros_smoothed <- croston_1266
bip1266SBA <- crost(bip1266[2], h = 5, type = 'sba', init = 'naive')
bip1266SBA
SBA_1266 <- bip1266SBA$frc.in
bip1266$SBA_smoothed <- SBA_1266
bip1266SBJ <- crost(bip1266[2], h = 5, type = 'sbj', init = 'naive')
bip1266SBJ
SBJ_1266 <- bip1266SBJ$frc.in
bip1266$SBJ_smoothed <- SBJ_1266

type_error_1266 <- cbind(croston_1266, SBA_1266, SBJ_1266)
dimensions_1266 <- dim(type_error_1266)[2]
smooth_data_1266 <- t(tcrossprod(rep(1,dimensions_1266),bip1266[,2]))
errors_1266 <- smooth_data_1266 - type_error_1266
errors_1266[is.na(errors_1266)] <- 0
ME_1266 <- apply(errors_1266, 2, mean)
MAE_1266 <- apply(abs(errors_1266), 2, mean)
RMSE_1266 <- sqrt(apply(errors_1266^2, 2, mean))
tot_err_1266 <- rbind(ME_1266, MAE_1266, RMSE_1266)
print(tot_err_1266)

plot(ts(bip1266$BIP001266, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1266, frequency = 52), col='red')
lines(ts(bip1266crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1266, frequency = 52), col='blue')
lines(ts(bip1266SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1266, frequency = 52), col='orange')
lines(ts(bip1266SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP7983#
bip7983 <- select(all_data, X, BIP007983)
bip7983crost <- crost(bip7983[2], h = 5, w = 0.15, init = "naive")
bip7983crost
croston_7983 <- bip7983crost$frc.in
bip7983$cros_smoothed <- croston_7983
bip7983SBA <- crost(bip7983[2], h = 5, type = 'sba', init = 'naive')
bip7983SBA
SBA_7983 <- bip7983SBA$frc.in
bip7983$SBA_smoothed <- SBA_7983
bip7983SBJ <- crost(bip7983[2], h = 5, type = 'sbj', init = 'naive')
bip7983SBJ
SBJ_7983 <- bip7983SBJ$frc.in
bip7983$SBJ_smoothed <- SBJ_7983

type_error_7983 <- cbind(croston_7983, SBA_7983, SBJ_7983)
dimensions_7983 <- dim(type_error_7983)[2]
smooth_data_7983 <- t(tcrossprod(rep(1,dimensions_7983),bip7983[,2]))
errors_7983 <- smooth_data_7983 - type_error_7983
errors_7983[is.na(errors_7983)] <- 0
ME_7983 <- apply(errors_7983, 2, mean)
MAE_7983 <- apply(abs(errors_7983), 2, mean)
RMSE_7983 <- sqrt(apply(errors_7983^2, 2, mean))
tot_err_7983 <- rbind(ME_7983, MAE_7983, RMSE_7983)
print(tot_err_7983)

plot(ts(bip7983$BIP007983, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_7983, frequency = 52), col='red')
lines(ts(bip7983crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_7983, frequency = 52), col='blue')
lines(ts(bip7983SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_7983, frequency = 52), col='orange')
lines(ts(bip7983SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston method for item BIP5889#
bip5889 <- select(all_data, X, BIP005889)
bip5889crost <- crost(bip5889[2], h = 5, w = 0.15, init = "naive")
bip5889crost
croston_5889 <- bip5889crost$frc.in
bip5889$cros_smoothed <- croston_5889
bip5889SBA <- crost(bip5889[2], h = 5, type = 'sba', init = 'naive')
bip5889SBA
SBA_5889 <- bip5889SBA$frc.in
bip5889$SBA_smoothed <- SBA_5889
bip5889SBJ <- crost(bip5889[2], h = 5, type = 'sbj', init = 'naive')
bip5889SBJ
SBJ_5889 <- bip5889SBJ$frc.in
bip5889$SBJ_smoothed <- SBJ_5889

type_error_5889 <- cbind(croston_5889, SBA_5889, SBJ_5889)
dimensions_5889 <- dim(type_error_5889)[2]
smooth_data_5889 <- t(tcrossprod(rep(1,dimensions_5889),bip5889[,2]))
errors_5889 <- smooth_data_5889 - type_error_5889
errors_5889[is.na(errors_5889)] <- 0
ME_5889 <- apply(errors_5889, 2, mean)
MAE_5889 <- apply(abs(errors_5889), 2, mean)
RMSE_5889 <- sqrt(apply(errors_5889^2, 2, mean))
tot_err_5889 <- rbind(ME_5889, MAE_5889, RMSE_5889)
print(tot_err_5889)

plot(ts(bip5889$BIP005889, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5889, frequency = 52), col='red')
lines(ts(bip5889crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5889, frequency = 52), col='blue')
lines(ts(bip5889SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5889, frequency = 52), col='orange')
lines(ts(bip5889SBJ$frc.out, start = c(1, 1.69)), col='purple')

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





