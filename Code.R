install.packages("dplyr")
install.packages("writexl")

library(forecast)
library(tsintermittent)
library(dplyr)
library(tibble)
library(writexl)

?crost

all_data <- read.csv("C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/AllData.csv", header = TRUE)


#Croston method, SBA, SBJ with aplha = 0.1 for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.1, init = "naive")
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, w= 0.1, type = 'sba', init = 'naive')
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, w = 0.1, type = 'sbj', init = "naive")
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271
bip1271_ses <- ses(bip1271[,2], h = 5, alpha = 0.1, initial = "simple")
SES_bip1271 <- bip1271_ses$fitted
SESpred_bip1271 <- bip1271_ses$mean
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")
SESpred_bip1271

type_error_1271 <- cbind(croston_1271, SBA_1271, SBJ_1271, SES_bip1271)
dimensions_1271 <- dim(type_error_1271)[2]
smooth_data_1271 <- t(tcrossprod(rep(1,dimensions_1271),bip1271[,2]))
errors_1271 <- smooth_data_1271 - type_error_1271
errors_1271[is.na(errors_1271)] <- 0
ME_1271 <- apply(errors_1271, 2, mean)
MAE_1271 <- apply(abs(errors_1271), 2, mean)
RMSE_1271 <- sqrt(apply(errors_1271^2, 2, mean))
tot_err_1271 <- rbind(ME_1271, MAE_1271, RMSE_1271)
print(tot_err_1271)

bip1271$err_croston <- errors_1271[,1]
bip1271$err_SBA <- errors_1271[,2]
bip1271$err_SBJ <- errors_1271[,3]
bip1271$err_SES <- errors_1271[,4]
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")

opar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 6))
plot(ts(bip1271$BIP001271, frequency = 52), main = "Forecast of item BIP001271", type = 'b', xlim = c(1, 1.8),
     ylab = "Demand", lwd = 2)
lines(ts(croston_1271, frequency = 52), col='red', lwd = 1)
lines(ts(bip1271crost$frc.out, start=c(1,1.69)), col='red', lwd = 1)

lines(ts(SBA_1271, frequency = 52), col='blue', lwd = 1)
lines(ts(bip1271SBA$frc.out, start = c(1,1.69)), col='blue', lwd = 1)

lines(ts(SBJ_1271, frequency = 52), col='purple', lwd = 1)
lines(ts(bip1271SBJ$frc.out, start = c(1, 1.69)), col='purple', lwd = 1)

lines(ts(SES, frequency = 52), col='cyan3', lwd = 1)
lines(ts(SESpred_bip1271, start = c(1,1.69)), col='cyan3', lwd = 1)
legend("bottomright", legend = c("Croston", "SBA", "SBJ", "Exp. smooth"), lwd = 1,
       col = c("red","blue","purple","cyan3"), xpd = TRUE)


View(bip1271)

write_xlsx(bip1271, "C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/bip1271_01_data.xlsx")


inter_crost_01 <- bip1271crost$components$c.in[,2]
interdemand_1271 <- data.frame(all_data[,1], inter_crost_01)
interdemand_1271$inter_sba_01 <- bip1271SBA$components$c.in[,2]
interdemand_1271$inter_sbj_01 <- bip1271SBJ$components$c.in[,2]


#Croston method, SBA, SBJ with aplha = 0.15 for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.15, init = "naive")
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, w= 0.15, type = 'sba', init = 'naive')
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, w = 0.15, type = 'sbj', init = "naive")
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271
bip1271_ses <- ses(bip1271[,2], h = 5, alpha = 0.15, initial = "simple")
SES_bip1271 <- bip1271_ses$fitted
SESpred_bip1271 <- bip1271_ses$mean
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")
SESpred_bip1271

type_error_1271 <- cbind(croston_1271, SBA_1271, SBJ_1271, SES_bip1271)
dimensions_1271 <- dim(type_error_1271)[2]
smooth_data_1271 <- t(tcrossprod(rep(1,dimensions_1271),bip1271[,2]))
errors_1271 <- smooth_data_1271 - type_error_1271
errors_1271[is.na(errors_1271)] <- 0
ME_1271 <- apply(errors_1271, 2, mean)
MAE_1271 <- apply(abs(errors_1271), 2, mean)
RMSE_1271 <- sqrt(apply(errors_1271^2, 2, mean))
tot_err_1271 <- rbind(ME_1271, MAE_1271, RMSE_1271)
print(tot_err_1271)

bip1271$err_croston <- errors_1271[,1]
bip1271$err_SBA <- errors_1271[,2]
bip1271$err_SBJ <- errors_1271[,3]
bip1271$err_SES <- errors_1271[,4]
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")

opar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 6))
plot(ts(bip1271$BIP001271, frequency = 52), main = "Forecast of item BIP001271", type = 'b', xlim = c(1, 1.8),
     ylab = "Demand", lwd = 2)
lines(ts(croston_1271, frequency = 52), col='red', lwd = 1)
lines(ts(bip1271crost$frc.out, start=c(1,1.69)), col='red', lwd = 1)

lines(ts(SBA_1271, frequency = 52), col='blue', lwd = 1)
lines(ts(bip1271SBA$frc.out, start = c(1,1.69)), col='blue', lwd = 1)

lines(ts(SBJ_1271, frequency = 52), col='purple', lwd = 1)
lines(ts(bip1271SBJ$frc.out, start = c(1, 1.69)), col='purple', lwd = 1)

lines(ts(SES, frequency = 52), col='cyan3', lwd = 1)
lines(ts(SESpred_bip1271, start = c(1,1.69)), col='cyan3', lwd = 1)
legend("bottomright", legend = c("Croston", "SBA", "SBJ", "Exp. smooth"), lwd = 1,
       col = c("red","blue","purple","cyan3"), xpd = TRUE)


View(bip1271)

write_xlsx(bip1271, "C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/bip1271_015_data.xlsx")

inter_crost_015 <- bip1271crost$components$c.in[,2]
interdemand_1271 <- data.frame(all_data[,1], inter_crost_015)
interdemand_1271$inter_sba_015 <- bip1271SBA$components$c.in[,2]
interdemand_1271$inter_sbj_015 <- bip1271SBJ$components$c.in[,2]


#Croston method, SBA, SBJ with aplha = 0.2 for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.2, init = "naive")
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, w= 0.2, type = 'sba', init = 'naive')
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, w = 0.2, type = 'sbj', init = "naive")
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271
bip1271_ses <- ses(bip1271[,2], h = 5, alpha = 0.2, initial = "simple")
SES_bip1271 <- bip1271_ses$fitted
SESpred_bip1271 <- bip1271_ses$mean
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")
SESpred_bip1271

type_error_1271 <- cbind(croston_1271, SBA_1271, SBJ_1271, SES_bip1271)
dimensions_1271 <- dim(type_error_1271)[2]
smooth_data_1271 <- t(tcrossprod(rep(1,dimensions_1271),bip1271[,2]))
errors_1271 <- smooth_data_1271 - type_error_1271
errors_1271[is.na(errors_1271)] <- 0
ME_1271 <- apply(errors_1271, 2, mean)
MAE_1271 <- apply(abs(errors_1271), 2, mean)
RMSE_1271 <- sqrt(apply(errors_1271^2, 2, mean))
tot_err_1271 <- rbind(ME_1271, MAE_1271, RMSE_1271)
print(tot_err_1271)

bip1271$err_croston <- errors_1271[,1]
bip1271$err_SBA <- errors_1271[,2]
bip1271$err_SBJ <- errors_1271[,3]
bip1271$err_SES <- errors_1271[,4]
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")

opar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 6))
plot(ts(bip1271$BIP001271, frequency = 52), main = "Forecast of item BIP001271", type = 'b', xlim = c(1, 1.8),
     ylab = "Demand", lwd = 2)
lines(ts(croston_1271, frequency = 52), col='red', lwd = 1)
lines(ts(bip1271crost$frc.out, start=c(1,1.69)), col='red', lwd = 1)

lines(ts(SBA_1271, frequency = 52), col='blue', lwd = 1)
lines(ts(bip1271SBA$frc.out, start = c(1,1.69)), col='blue', lwd = 1)

lines(ts(SBJ_1271, frequency = 52), col='purple', lwd = 1)
lines(ts(bip1271SBJ$frc.out, start = c(1, 1.69)), col='purple', lwd = 1)

lines(ts(SES_bip1271, frequency = 52), col='cyan3', lwd = 1)
lines(ts(SESpred_bip1271, start = c(1,1.69)), col='cyan3', lwd = 1)
legend("bottomright", legend = c("Croston", "SBA", "SBJ", "Exp. smooth"), lwd = 1,
       col = c("red","blue","purple","cyan3"), xpd = TRUE)


View(bip1271)

inter_crost_015 <- bip1271crost$components$c.in[,2]
interdemand_1271 <- data.frame(all_data[,1], inter_crost_015)
interdemand_1271$inter_sba_015 <- bip1271SBA$components$c.in[,2]
interdemand_1271$inter_sbj_015 <- bip1271SBJ$components$c.in[,2]


write_xlsx(bip1271, "C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/bip1271_02_data.xlsx")

#Croston method, SBA, SBJ with aplha = 0.5 for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, w = 0.5, init = "naive")
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, w= 0.5, type = 'sba', init = 'naive')
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, w = 0.5, type = 'sbj', init = "naive")
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271
bip1271_ses <- ses(bip1271[,2], h = 5, alpha = 0.5, initial = "simple")
SES_bip1271 <- bip1271_ses$fitted
SESpred_bip1271 <- bip1271_ses$mean
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")
SESpred_bip1271

type_error_1271 <- cbind(croston_1271, SBA_1271, SBJ_1271, SES_bip1271)
dimensions_1271 <- dim(type_error_1271)[2]
smooth_data_1271 <- t(tcrossprod(rep(1,dimensions_1271),bip1271[,2]))
errors_1271 <- smooth_data_1271 - type_error_1271
errors_1271[is.na(errors_1271)] <- 0
ME_1271 <- apply(errors_1271, 2, mean)
MAE_1271 <- apply(abs(errors_1271), 2, mean)
RMSE_1271 <- sqrt(apply(errors_1271^2, 2, mean))
tot_err_1271 <- rbind(ME_1271, MAE_1271, RMSE_1271)
print(tot_err_1271)

bip1271$err_croston <- errors_1271[,1]
bip1271$err_SBA <- errors_1271[,2]
bip1271$err_SBJ <- errors_1271[,3]
bip1271$err_SES <- errors_1271[,4]
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")

opar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 6))
plot(ts(bip1271$BIP001271, frequency = 52), main = "Forecast of item BIP001271", type = 'b', xlim = c(1, 1.8),
     ylab = "Demand", lwd = 2)
lines(ts(croston_1271, frequency = 52), col='red', lwd = 1)
lines(ts(bip1271crost$frc.out, start=c(1,1.69)), col='red', lwd = 1)

lines(ts(SBA_1271, frequency = 52), col='blue', lwd = 1)
lines(ts(bip1271SBA$frc.out, start = c(1,1.69)), col='blue', lwd = 1)

lines(ts(SBJ_1271, frequency = 52), col='purple', lwd = 1)
lines(ts(bip1271SBJ$frc.out, start = c(1, 1.69)), col='purple', lwd = 1)

lines(ts(SES_bip1271, frequency = 52), col='cyan3', lwd = 1)
lines(ts(SESpred_bip1271, start = c(1,1.69)), col='cyan3', lwd = 1)
legend("bottomright", legend = c("Croston", "SBA", "SBJ", "Exp. smooth"), lwd = 1,
       col = c("red","blue","purple","cyan3"), xpd = TRUE)


View(bip1271)

write_xlsx(bip1271, "C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/bip1271_05_data.xlsx")

inter_crost_05 <- bip1271crost$components$c.in[,2]
interdemand_1271 <- data.frame(all_data[,1], inter_crost_05)
interdemand_1271$inter_sba_05 <- bip1271SBA$components$c.in[,2]
interdemand_1271$inter_sbj_05 <- bip1271SBJ$components$c.in[,2]

#Croston method, SBA, SBJ with "optimal" alpha for item BIP1271#
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5, init = "naive")
bip1271crost
croston_1271 <- bip1271crost$frc.in
bip1271$cros_smoothed <- croston_1271
bip1271SBA <- crost(bip1271[2], h = 5, type = 'sba', init = 'naive')
bip1271SBA
SBA_1271 <- bip1271SBA$frc.in
bip1271$SBA_smoothed <- SBA_1271
bip1271SBJ <- crost(bip1271[2], h = 5, type = 'sbj', init = "naive")
bip1271SBJ
SBJ_1271 <- bip1271SBJ$frc.in
bip1271$SBJ_smoothed <- SBJ_1271
bip1271_ses <- ses(bip1271[,2], h = 5, initial = "simple")
SES_bip1271 <- bip1271_ses$fitted
SESpred_bip1271 <- bip1271_ses$mean
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")
SESpred_bip1271
bip1271_ses$model

type_error_1271 <- cbind(croston_1271, SBA_1271, SBJ_1271, SES_bip1271)
dimensions_1271 <- dim(type_error_1271)[2]
smooth_data_1271 <- t(tcrossprod(rep(1,dimensions_1271),bip1271[,2]))
errors_1271 <- smooth_data_1271 - type_error_1271
errors_1271[is.na(errors_1271)] <- 0
ME_1271 <- apply(errors_1271, 2, mean)
MAE_1271 <- apply(abs(errors_1271), 2, mean)
RMSE_1271 <- sqrt(apply(errors_1271^2, 2, mean))
tot_err_1271 <- rbind(ME_1271, MAE_1271, RMSE_1271)
print(tot_err_1271)

bip1271$err_croston <- errors_1271[,1]
bip1271$err_SBA <- errors_1271[,2]
bip1271$err_SBJ <- errors_1271[,3]
bip1271$err_SES <- errors_1271[,4]
bip1271 <- add_column(bip1271, SES_bip1271, .after = "BIP001271")

opar <- par(no.readonly = TRUE)
par(mar=c(5.1, 4.1, 4.1, 6))
plot(ts(bip1271$BIP001271, frequency = 52), main = "Forecast of item BIP001271", type = 'b', xlim = c(1, 1.8),
     ylab = "Demand", lwd = 2)
lines(ts(croston_1271, frequency = 52), col='red', lwd = 1)
lines(ts(bip1271crost$frc.out, start=c(1,1.69)), col='red', lwd = 1)

lines(ts(SBA_1271, frequency = 52), col='blue', lwd = 1)
lines(ts(bip1271SBA$frc.out, start = c(1,1.69)), col='blue', lwd = 1)

lines(ts(SBJ_1271, frequency = 52), col='purple', lwd = 1)
lines(ts(bip1271SBJ$frc.out, start = c(1, 1.69)), col='purple', lwd = 1)

lines(ts(SES_bip1271, frequency = 52), col='cyan3', lwd = 1)
lines(ts(SESpred_bip1271, start = c(1,1.69)), col='cyan3', lwd = 1)
legend("bottomright", legend = c("Croston", "SBA", "SBJ", "Exp. smooth"), lwd = 1,
       col = c("red","blue","purple","cyan3"), xpd = TRUE)


View(bip1271)

write_xlsx(bip1271, "C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/bip1271_opt_data.xlsx")

inter_crost_opt <- bip1271crost$components$c.in[,2]
interdemand_1271 <- data.frame(all_data[,1], inter_crost_opt)
interdemand_1271$inter_sba_opt <- bip1271SBA$components$c.in[,2]
interdemand_1271$inter_sbj_opt <- bip1271SBJ$components$c.in[,2]
interdemand_1271

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
bip8645crost <- crost(bip8645[2], h = 5, w = 0.15, init = "naive")
bip8645crost
croston_8645 <- bip8645crost$frc.in
bip8645$cros_smoothed <- croston_8645
bip8645SBA <- crost(bip8645[2], h = 5, type = 'sba', init = 'naive')
bip8645SBA
SBA_8645 <- bip8645SBA$frc.in
bip8645$SBA_smoothed <- SBA_8645
bip8645SBJ <- crost(bip8645[2], h = 5, type = 'sbj', init = 'naive')
bip8645SBJ
SBJ_8645 <- bip8645SBJ$frc.in
bip8645$SBJ_smoothed <- SBJ_8645

type_error_8645 <- cbind(croston_8645, SBA_8645, SBJ_8645)
dimensions_8645 <- dim(type_error_8645)[2]
smooth_data_8645 <- t(tcrossprod(rep(1,dimensions_8645),bip8645[,2]))
errors_8645 <- smooth_data_8645 - type_error_8645
errors_8645[is.na(errors_8645)] <- 0
ME_8645 <- apply(errors_8645, 2, mean)
MAE_8645 <- apply(abs(errors_8645), 2, mean)
RMSE_8645 <- sqrt(apply(errors_8645^2, 2, mean))
tot_err_8645 <- rbind(ME_8645, MAE_8645, RMSE_8645)
print(tot_err_8645)

plot(ts(bip8645$BIP008645, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_8645, frequency = 52), col='red')
lines(ts(bip8645crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_8645, frequency = 52), col='blue')
lines(ts(bip8645SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_8645, frequency = 52), col='orange')
lines(ts(bip8645SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5467#
bip5467 <- select(all_data, X, BIP005467)
bip5467crost <- crost(bip5467[2], h = 5, w = 0.15, init = "naive")
bip5467crost
croston_5467 <- bip5467crost$frc.in
bip5467$cros_smoothed <- croston_5467
bip5467SBA <- crost(bip5467[2], h = 5, type = 'sba', init = 'naive')
bip5467SBA
SBA_5467 <- bip5467SBA$frc.in
bip5467$SBA_smoothed <- SBA_5467
bip5467SBJ <- crost(bip5467[2], h = 5, type = 'sbj', init = 'naive')
bip5467SBJ
SBJ_5467 <- bip5467SBJ$frc.in
bip5467$SBJ_smoothed <- SBJ_5467

type_error_5467 <- cbind(croston_5467, SBA_5467, SBJ_5467)
dimensions_5467 <- dim(type_error_5467)[2]
smooth_data_5467 <- t(tcrossprod(rep(1,dimensions_5467),bip5467[,2]))
errors_5467 <- smooth_data_5467 - type_error_5467
errors_5467[is.na(errors_5467)] <- 0
ME_5467 <- apply(errors_5467, 2, mean)
MAE_5467 <- apply(abs(errors_5467), 2, mean)
RMSE_5467 <- sqrt(apply(errors_5467^2, 2, mean))
tot_err_5467 <- rbind(ME_5467, MAE_5467, RMSE_5467)
print(tot_err_5467)

plot(ts(bip5467$BIP005467, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5467, frequency = 52), col='red')
lines(ts(bip5467crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5467, frequency = 52), col='blue')
lines(ts(bip5467SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5467, frequency = 52), col='orange')
lines(ts(bip5467SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP8920#
bip8920 <- select(all_data, X, BIP008920)
bip8920crost <- crost(bip8920[2], h = 5, w = 0.15, init = "naive")
bip8920crost
croston_8920 <- bip8920crost$frc.in
bip8920$cros_smoothed <- croston_8920
bip8920SBA <- crost(bip8920[2], h = 5, type = 'sba', init = 'naive')
bip8920SBA
SBA_8920 <- bip8920SBA$frc.in
bip8920$SBA_smoothed <- SBA_8920
bip8920SBJ <- crost(bip8920[2], h = 5, type = 'sbj', init = 'naive')
bip8920SBJ
SBJ_8920 <- bip8920SBJ$frc.in
bip8920$SBJ_smoothed <- SBJ_8920

type_error_8920 <- cbind(croston_8920, SBA_8920, SBJ_8920)
dimensions_8920 <- dim(type_error_8920)[2]
smooth_data_8920 <- t(tcrossprod(rep(1,dimensions_8920),bip8920[,2]))
errors_8920 <- smooth_data_8920 - type_error_8920
errors_8920[is.na(errors_8920)] <- 0
ME_8920 <- apply(errors_8920, 2, mean)
MAE_8920 <- apply(abs(errors_8920), 2, mean)
RMSE_8920 <- sqrt(apply(errors_8920^2, 2, mean))
tot_err_8920 <- rbind(ME_8920, MAE_8920, RMSE_8920)
print(tot_err_8920)

plot(ts(bip8920$BIP008920, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_8920, frequency = 52), col='red')
lines(ts(bip8920crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_8920, frequency = 52), col='blue')
lines(ts(bip8920SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_8920, frequency = 52), col='orange')
lines(ts(bip8920SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP9107#
bip9107 <- select(all_data, X, BIP009107)
bip9107crost <- crost(bip9107[2], h = 5, w = 0.15, init = "naive")
bip9107crost
croston_9107 <- bip9107crost$frc.in
bip9107$cros_smoothed <- croston_9107
bip9107SBA <- crost(bip9107[2], h = 5, type = 'sba', init = 'naive')
bip9107SBA
SBA_9107 <- bip9107SBA$frc.in
bip9107$SBA_smoothed <- SBA_9107
bip9107SBJ <- crost(bip9107[2], h = 5, type = 'sbj', init = 'naive')
bip9107SBJ
SBJ_9107 <- bip9107SBJ$frc.in
bip9107$SBJ_smoothed <- SBJ_9107

type_error_9107 <- cbind(croston_9107, SBA_9107, SBJ_9107)
dimensions_9107 <- dim(type_error_9107)[2]
smooth_data_9107 <- t(tcrossprod(rep(1,dimensions_9107),bip9107[,2]))
errors_9107 <- smooth_data_9107 - type_error_9107
errors_9107[is.na(errors_9107)] <- 0
ME_9107 <- apply(errors_9107, 2, mean)
MAE_9107 <- apply(abs(errors_9107), 2, mean)
RMSE_9107 <- sqrt(apply(errors_9107^2, 2, mean))
tot_err_9107 <- rbind(ME_9107, MAE_9107, RMSE_9107)
print(tot_err_9107)

plot(ts(bip9107$BIP009107, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_9107, frequency = 52), col='red')
lines(ts(bip9107crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_9107, frequency = 52), col='blue')
lines(ts(bip9107SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_9107, frequency = 52), col='orange')
lines(ts(bip9107SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP6806#
bip6806 <- select(all_data, X, BIP006806)
bip6806crost <- crost(bip6806[2], h = 5, w = 0.15, init = "naive")
bip6806crost
croston_6806 <- bip6806crost$frc.in
bip6806$cros_smoothed <- croston_6806
bip6806SBA <- crost(bip6806[2], h = 5, type = 'sba', init = 'naive')
bip6806SBA
SBA_6806 <- bip6806SBA$frc.in
bip6806$SBA_smoothed <- SBA_6806
bip6806SBJ <- crost(bip6806[2], h = 5, type = 'sbj', init = 'naive')
bip6806SBJ
SBJ_6806 <- bip6806SBJ$frc.in
bip6806$SBJ_smoothed <- SBJ_6806

type_error_6806 <- cbind(croston_6806, SBA_6806, SBJ_6806)
dimensions_6806 <- dim(type_error_6806)[2]
smooth_data_6806 <- t(tcrossprod(rep(1,dimensions_6806),bip6806[,2]))
errors_6806 <- smooth_data_6806 - type_error_6806
errors_6806[is.na(errors_6806)] <- 0
ME_6806 <- apply(errors_6806, 2, mean)
MAE_6806 <- apply(abs(errors_6806), 2, mean)
RMSE_6806 <- sqrt(apply(errors_6806^2, 2, mean))
tot_err_6806 <- rbind(ME_6806, MAE_6806, RMSE_6806)
print(tot_err_6806)

plot(ts(bip6806$BIP006806, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_6806, frequency = 52), col='red')
lines(ts(bip6806crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_6806, frequency = 52), col='blue')
lines(ts(bip6806SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_6806, frequency = 52), col='orange')
lines(ts(bip6806SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5887#
bip5887 <- select(all_data, X, BIP005887)
bip5887crost <- crost(bip5887[2], h = 5, w = 0.15, init = "naive")
bip5887crost
croston_5887 <- bip5887crost$frc.in
bip5887$cros_smoothed <- croston_5887
bip5887SBA <- crost(bip5887[2], h = 5, type = 'sba', init = 'naive')
bip5887SBA
SBA_5887 <- bip5887SBA$frc.in
bip5887$SBA_smoothed <- SBA_5887
bip5887SBJ <- crost(bip5887[2], h = 5, type = 'sbj', init = 'naive')
bip5887SBJ
SBJ_5887 <- bip5887SBJ$frc.in
bip5887$SBJ_smoothed <- SBJ_5887

type_error_5887 <- cbind(croston_5887, SBA_5887, SBJ_5887)
dimensions_5887 <- dim(type_error_5887)[2]
smooth_data_5887 <- t(tcrossprod(rep(1,dimensions_5887),bip5887[,2]))
errors_5887 <- smooth_data_5887 - type_error_5887
errors_5887[is.na(errors_5887)] <- 0
ME_5887 <- apply(errors_5887, 2, mean)
MAE_5887 <- apply(abs(errors_5887), 2, mean)
RMSE_5887 <- sqrt(apply(errors_5887^2, 2, mean))
tot_err_5887 <- rbind(ME_5887, MAE_5887, RMSE_5887)
print(tot_err_5887)

plot(ts(bip5887$BIP005887, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5887, frequency = 52), col='red')
lines(ts(bip5887crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5887, frequency = 52), col='blue')
lines(ts(bip5887SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5887, frequency = 52), col='orange')
lines(ts(bip5887SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP8013#
bip8013 <- select(all_data, X, BIP008013)
bip8013crost <- crost(bip8013[2], h = 5, w = 0.15, init = "naive")
bip8013crost
croston_8013 <- bip8013crost$frc.in
bip8013$cros_smoothed <- croston_8013
bip8013SBA <- crost(bip8013[2], h = 5, type = 'sba', init = 'naive')
bip8013SBA
SBA_8013 <- bip8013SBA$frc.in
bip8013$SBA_smoothed <- SBA_8013
bip8013SBJ <- crost(bip8013[2], h = 5, type = 'sbj', init = 'naive')
bip8013SBJ
SBJ_8013 <- bip8013SBJ$frc.in
bip8013$SBJ_smoothed <- SBJ_8013

type_error_8013 <- cbind(croston_8013, SBA_8013, SBJ_8013)
dimensions_8013 <- dim(type_error_8013)[2]
smooth_data_8013 <- t(tcrossprod(rep(1,dimensions_8013),bip8013[,2]))
errors_8013 <- smooth_data_8013 - type_error_8013
errors_8013[is.na(errors_8013)] <- 0
ME_8013 <- apply(errors_8013, 2, mean)
MAE_8013 <- apply(abs(errors_8013), 2, mean)
RMSE_8013 <- sqrt(apply(errors_8013^2, 2, mean))
tot_err_8013 <- rbind(ME_8013, MAE_8013, RMSE_8013)
print(tot_err_8013)

plot(ts(bip8013$BIP008013, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_8013, frequency = 52), col='red')
lines(ts(bip8013crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_8013, frequency = 52), col='blue')
lines(ts(bip8013SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_8013, frequency = 52), col='orange')
lines(ts(bip8013SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP3816#
bip3816 <- select(all_data, X, BIP003816)
bip3816crost <- crost(bip3816[2], h = 5, w = 0.15, init = "naive")
bip3816crost
croston_3816 <- bip3816crost$frc.in
bip3816$cros_smoothed <- croston_3816
bip3816SBA <- crost(bip3816[2], h = 5, type = 'sba', init = 'naive')
bip3816SBA
SBA_3816 <- bip3816SBA$frc.in
bip3816$SBA_smoothed <- SBA_3816
bip3816SBJ <- crost(bip3816[2], h = 5, type = 'sbj', init = 'naive')
bip3816SBJ
SBJ_3816 <- bip3816SBJ$frc.in
bip3816$SBJ_smoothed <- SBJ_3816

type_error_3816 <- cbind(croston_3816, SBA_3816, SBJ_3816)
dimensions_3816 <- dim(type_error_3816)[2]
smooth_data_3816 <- t(tcrossprod(rep(1,dimensions_3816),bip3816[,2]))
errors_3816 <- smooth_data_3816 - type_error_3816
errors_3816[is.na(errors_3816)] <- 0
ME_3816 <- apply(errors_3816, 2, mean)
MAE_3816 <- apply(abs(errors_3816), 2, mean)
RMSE_3816 <- sqrt(apply(errors_3816^2, 2, mean))
tot_err_3816 <- rbind(ME_3816, MAE_3816, RMSE_3816)
print(tot_err_3816)

plot(ts(bip3816$BIP003816, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_3816, frequency = 52), col='red')
lines(ts(bip3816crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_3816, frequency = 52), col='blue')
lines(ts(bip3816SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_3816, frequency = 52), col='orange')
lines(ts(bip3816SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP7269#
bip7269 <- select(all_data, X, BIP007269)
bip7269crost <- crost(bip7269[2], h = 5, w = 0.15, init = "naive")
bip7269crost
croston_7269 <- bip7269crost$frc.in
bip7269$cros_smoothed <- croston_7269
bip7269SBA <- crost(bip7269[2], h = 5, type = 'sba', init = 'naive')
bip7269SBA
SBA_7269 <- bip7269SBA$frc.in
bip7269$SBA_smoothed <- SBA_7269
bip7269SBJ <- crost(bip7269[2], h = 5, type = 'sbj', init = 'naive')
bip7269SBJ
SBJ_7269 <- bip7269SBJ$frc.in
bip7269$SBJ_smoothed <- SBJ_7269

type_error_7269 <- cbind(croston_7269, SBA_7269, SBJ_7269)
dimensions_7269 <- dim(type_error_7269)[2]
smooth_data_7269 <- t(tcrossprod(rep(1,dimensions_7269),bip7269[,2]))
errors_7269 <- smooth_data_7269 - type_error_7269
errors_7269[is.na(errors_7269)] <- 0
ME_7269 <- apply(errors_7269, 2, mean)
MAE_7269 <- apply(abs(errors_7269), 2, mean)
RMSE_7269 <- sqrt(apply(errors_7269^2, 2, mean))
tot_err_7269 <- rbind(ME_7269, MAE_7269, RMSE_7269)
print(tot_err_7269)

plot(ts(bip7269$BIP007269, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_7269, frequency = 52), col='red')
lines(ts(bip7269crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_7269, frequency = 52), col='blue')
lines(ts(bip7269SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_7269, frequency = 52), col='orange')
lines(ts(bip7269SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP2846#
bip2846 <- select(all_data, X, BIP002846)
bip2846crost <- crost(bip2846[2], h = 5, w = 0.15, init = "naive")
bip2846crost
croston_2846 <- bip2846crost$frc.in
bip2846$cros_smoothed <- croston_2846
bip2846SBA <- crost(bip2846[2], h = 5, type = 'sba', init = 'naive')
bip2846SBA
SBA_2846 <- bip2846SBA$frc.in
bip2846$SBA_smoothed <- SBA_2846
bip2846SBJ <- crost(bip2846[2], h = 5, type = 'sbj', init = 'naive')
bip2846SBJ
SBJ_2846 <- bip2846SBJ$frc.in
bip2846$SBJ_smoothed <- SBJ_2846

type_error_2846 <- cbind(croston_2846, SBA_2846, SBJ_2846)
dimensions_2846 <- dim(type_error_2846)[2]
smooth_data_2846 <- t(tcrossprod(rep(1,dimensions_2846),bip2846[,2]))
errors_2846 <- smooth_data_2846 - type_error_2846
errors_2846[is.na(errors_2846)] <- 0
ME_2846 <- apply(errors_2846, 2, mean)
MAE_2846 <- apply(abs(errors_2846), 2, mean)
RMSE_2846 <- sqrt(apply(errors_2846^2, 2, mean))
tot_err_2846 <- rbind(ME_2846, MAE_2846, RMSE_2846)
print(tot_err_2846)

plot(ts(bip2846$BIP002846, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2846, frequency = 52), col='red')
lines(ts(bip2846crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2846, frequency = 52), col='blue')
lines(ts(bip2846SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2846, frequency = 52), col='orange')
lines(ts(bip2846SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP7495#
bip7495 <- select(all_data, X, BIP007495)
bip7495crost <- crost(bip7495[2], h = 5, w = 0.15, init = "naive")
bip7495crost
croston_7495 <- bip7495crost$frc.in
bip7495$cros_smoothed <- croston_7495
bip7495SBA <- crost(bip7495[2], h = 5, type = 'sba', init = 'naive')
bip7495SBA
SBA_7495 <- bip7495SBA$frc.in
bip7495$SBA_smoothed <- SBA_7495
bip7495SBJ <- crost(bip7495[2], h = 5, type = 'sbj', init = 'naive')
bip7495SBJ
SBJ_7495 <- bip7495SBJ$frc.in
bip7495$SBJ_smoothed <- SBJ_7495

type_error_7495 <- cbind(croston_7495, SBA_7495, SBJ_7495)
dimensions_7495 <- dim(type_error_7495)[2]
smooth_data_7495 <- t(tcrossprod(rep(1,dimensions_7495),bip7495[,2]))
errors_7495 <- smooth_data_7495 - type_error_7495
errors_7495[is.na(errors_7495)] <- 0
ME_7495 <- apply(errors_7495, 2, mean)
MAE_7495 <- apply(abs(errors_7495), 2, mean)
RMSE_7495 <- sqrt(apply(errors_7495^2, 2, mean))
tot_err_7495 <- rbind(ME_7495, MAE_7495, RMSE_7495)
print(tot_err_7495)

plot(ts(bip7495$BIP007495, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_7495, frequency = 52), col='red')
lines(ts(bip7495crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_7495, frequency = 52), col='blue')
lines(ts(bip7495SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_7495, frequency = 52), col='orange')
lines(ts(bip7495SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP1478#
bip1478 <- select(all_data, X, BIP001478)
bip1478crost <- crost(bip1478[2], h = 5, w = 0.15, init = "naive")
bip1478crost
croston_1478 <- bip1478crost$frc.in
bip1478$cros_smoothed <- croston_1478
bip1478SBA <- crost(bip1478[2], h = 5, type = 'sba', init = 'naive')
bip1478SBA
SBA_1478 <- bip1478SBA$frc.in
bip1478$SBA_smoothed <- SBA_1478
bip1478SBJ <- crost(bip1478[2], h = 5, type = 'sbj', init = 'naive')
bip1478SBJ
SBJ_1478 <- bip1478SBJ$frc.in
bip1478$SBJ_smoothed <- SBJ_1478

type_error_1478 <- cbind(croston_1478, SBA_1478, SBJ_1478)
dimensions_1478 <- dim(type_error_1478)[2]
smooth_data_1478 <- t(tcrossprod(rep(1,dimensions_1478),bip1478[,2]))
errors_1478 <- smooth_data_1478 - type_error_1478
errors_1478[is.na(errors_1478)] <- 0
ME_1478 <- apply(errors_1478, 2, mean)
MAE_1478 <- apply(abs(errors_1478), 2, mean)
RMSE_1478 <- sqrt(apply(errors_1478^2, 2, mean))
tot_err_1478 <- rbind(ME_1478, MAE_1478, RMSE_1478)
print(tot_err_1478)

plot(ts(bip1478$BIP001478, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1478, frequency = 52), col='red')
lines(ts(bip1478crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1478, frequency = 52), col='blue')
lines(ts(bip1478SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1478, frequency = 52), col='orange')
lines(ts(bip1478SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP1479#
bip1479 <- select(all_data, X, BIP001479)
bip1479crost <- crost(bip1479[2], h = 5, w = 0.15, init = "naive")
bip1479crost
croston_1479 <- bip1479crost$frc.in
bip1479$cros_smoothed <- croston_1479
bip1479SBA <- crost(bip1479[2], h = 5, type = 'sba', init = 'naive')
bip1479SBA
SBA_1479 <- bip1479SBA$frc.in
bip1479$SBA_smoothed <- SBA_1479
bip1479SBJ <- crost(bip1479[2], h = 5, type = 'sbj', init = 'naive')
bip1479SBJ
SBJ_1479 <- bip1479SBJ$frc.in
bip1479$SBJ_smoothed <- SBJ_1479

type_error_1479 <- cbind(croston_1479, SBA_1479, SBJ_1479)
dimensions_1479 <- dim(type_error_1479)[2]
smooth_data_1479 <- t(tcrossprod(rep(1,dimensions_1479),bip1479[,2]))
errors_1479 <- smooth_data_1479 - type_error_1479
errors_1479[is.na(errors_1479)] <- 0
ME_1479 <- apply(errors_1479, 2, mean)
MAE_1479 <- apply(abs(errors_1479), 2, mean)
RMSE_1479 <- sqrt(apply(errors_1479^2, 2, mean))
tot_err_1479 <- rbind(ME_1479, MAE_1479, RMSE_1479)
print(tot_err_1479)

plot(ts(bip1479$BIP001479, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1479, frequency = 52), col='red')
lines(ts(bip1479crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1479, frequency = 52), col='blue')
lines(ts(bip1479SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1479, frequency = 52), col='orange')
lines(ts(bip1479SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP4338#
bip4338 <- select(all_data, X, BIP004338)
bip4338crost <- crost(bip4338[2], h = 5, w = 0.15, init = "naive")
bip4338crost
croston_4338 <- bip4338crost$frc.in
bip4338$cros_smoothed <- croston_4338
bip4338SBA <- crost(bip4338[2], h = 5, type = 'sba', init = 'naive')
bip4338SBA
SBA_4338 <- bip4338SBA$frc.in
bip4338$SBA_smoothed <- SBA_4338
bip4338SBJ <- crost(bip4338[2], h = 5, type = 'sbj', init = 'naive')
bip4338SBJ
SBJ_4338 <- bip4338SBJ$frc.in
bip4338$SBJ_smoothed <- SBJ_4338

type_error_4338 <- cbind(croston_4338, SBA_4338, SBJ_4338)
dimensions_4338 <- dim(type_error_4338)[2]
smooth_data_4338 <- t(tcrossprod(rep(1,dimensions_4338),bip4338[,2]))
errors_4338 <- smooth_data_4338 - type_error_4338
errors_4338[is.na(errors_4338)] <- 0
ME_4338 <- apply(errors_4338, 2, mean)
MAE_4338 <- apply(abs(errors_4338), 2, mean)
RMSE_4338 <- sqrt(apply(errors_4338^2, 2, mean))
tot_err_4338 <- rbind(ME_4338, MAE_4338, RMSE_4338)
print(tot_err_4338)

plot(ts(bip4338$BIP004338, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_4338, frequency = 52), col='red')
lines(ts(bip4338crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_4338, frequency = 52), col='blue')
lines(ts(bip4338SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_4338, frequency = 52), col='orange')
lines(ts(bip4338SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP4394#
bip4394 <- select(all_data, X, BIP004394)
bip4394crost <- crost(bip4394[2], h = 5, w = 0.15, init = "naive")
bip4394crost
croston_4394 <- bip4394crost$frc.in
bip4394$cros_smoothed <- croston_4394
bip4394SBA <- crost(bip4394[2], h = 5, type = 'sba', init = 'naive')
bip4394SBA
SBA_4394 <- bip4394SBA$frc.in
bip4394$SBA_smoothed <- SBA_4394
bip4394SBJ <- crost(bip4394[2], h = 5, type = 'sbj', init = 'naive')
bip4394SBJ
SBJ_4394 <- bip4394SBJ$frc.in
bip4394$SBJ_smoothed <- SBJ_4394

type_error_4394 <- cbind(croston_4394, SBA_4394, SBJ_4394)
dimensions_4394 <- dim(type_error_4394)[2]
smooth_data_4394 <- t(tcrossprod(rep(1,dimensions_4394),bip4394[,2]))
errors_4394 <- smooth_data_4394 - type_error_4394
errors_4394[is.na(errors_4394)] <- 0
ME_4394 <- apply(errors_4394, 2, mean)
MAE_4394 <- apply(abs(errors_4394), 2, mean)
RMSE_4394 <- sqrt(apply(errors_4394^2, 2, mean))
tot_err_4394 <- rbind(ME_4394, MAE_4394, RMSE_4394)
print(tot_err_4394)

plot(ts(bip4394$BIP004394, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_4394, frequency = 52), col='red')
lines(ts(bip4394crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_4394, frequency = 52), col='blue')
lines(ts(bip4394SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_4394, frequency = 52), col='orange')
lines(ts(bip4394SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP8641#
bip8641 <- select(all_data, X, BIP008641)
bip8641crost <- crost(bip8641[2], h = 5, w = 0.15, init = "naive")
bip8641crost
croston_8641 <- bip8641crost$frc.in
bip8641$cros_smoothed <- croston_8641
bip8641SBA <- crost(bip8641[2], h = 5, type = 'sba', init = 'naive')
bip8641SBA
SBA_8641 <- bip8641SBA$frc.in
bip8641$SBA_smoothed <- SBA_8641
bip8641SBJ <- crost(bip8641[2], h = 5, type = 'sbj', init = 'naive')
bip8641SBJ
SBJ_8641 <- bip8641SBJ$frc.in
bip8641$SBJ_smoothed <- SBJ_8641

type_error_8641 <- cbind(croston_8641, SBA_8641, SBJ_8641)
dimensions_8641 <- dim(type_error_8641)[2]
smooth_data_8641 <- t(tcrossprod(rep(1,dimensions_8641),bip8641[,2]))
errors_8641 <- smooth_data_8641 - type_error_8641
errors_8641[is.na(errors_8641)] <- 0
ME_8641 <- apply(errors_8641, 2, mean)
MAE_8641 <- apply(abs(errors_8641), 2, mean)
RMSE_8641 <- sqrt(apply(errors_8641^2, 2, mean))
tot_err_8641 <- rbind(ME_8641, MAE_8641, RMSE_8641)
print(tot_err_8641)

plot(ts(bip8641$BIP008641, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_8641, frequency = 52), col='red')
lines(ts(bip8641crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_8641, frequency = 52), col='blue')
lines(ts(bip8641SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_8641, frequency = 52), col='orange')
lines(ts(bip8641SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP2861#
bip2861 <- select(all_data, X, BIP002861)
bip2861crost <- crost(bip2861[2], h = 5, w = 0.15, init = "naive")
bip2861crost
croston_2861 <- bip2861crost$frc.in
bip2861$cros_smoothed <- croston_2861
bip2861SBA <- crost(bip2861[2], h = 5, type = 'sba', init = 'naive')
bip2861SBA
SBA_2861 <- bip2861SBA$frc.in
bip2861$SBA_smoothed <- SBA_2861
bip2861SBJ <- crost(bip2861[2], h = 5, type = 'sbj', init = 'naive')
bip2861SBJ
SBJ_2861 <- bip2861SBJ$frc.in
bip2861$SBJ_smoothed <- SBJ_2861

type_error_2861 <- cbind(croston_2861, SBA_2861, SBJ_2861)
dimensions_2861 <- dim(type_error_2861)[2]
smooth_data_2861 <- t(tcrossprod(rep(1,dimensions_2861),bip2861[,2]))
errors_2861 <- smooth_data_2861 - type_error_2861
errors_2861[is.na(errors_2861)] <- 0
ME_2861 <- apply(errors_2861, 2, mean)
MAE_2861 <- apply(abs(errors_2861), 2, mean)
RMSE_2861 <- sqrt(apply(errors_2861^2, 2, mean))
tot_err_2861 <- rbind(ME_2861, MAE_2861, RMSE_2861)
print(tot_err_2861)

plot(ts(bip2861$BIP002861, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2861, frequency = 52), col='red')
lines(ts(bip2861crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2861, frequency = 52), col='blue')
lines(ts(bip2861SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2861, frequency = 52), col='orange')
lines(ts(bip2861SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP1480#
bip1480 <- select(all_data, X, BIP001480)
bip1480crost <- crost(bip1480[2], h = 5, w = 0.15, init = "naive")
bip1480crost
croston_1480 <- bip1480crost$frc.in
bip1480$cros_smoothed <- croston_1480
bip1480SBA <- crost(bip1480[2], h = 5, type = 'sba', init = 'naive')
bip1480SBA
SBA_1480 <- bip1480SBA$frc.in
bip1480$SBA_smoothed <- SBA_1480
bip1480SBJ <- crost(bip1480[2], h = 5, type = 'sbj', init = 'naive')
bip1480SBJ
SBJ_1480 <- bip1480SBJ$frc.in
bip1480$SBJ_smoothed <- SBJ_1480

type_error_1480 <- cbind(croston_1480, SBA_1480, SBJ_1480)
dimensions_1480 <- dim(type_error_1480)[2]
smooth_data_1480 <- t(tcrossprod(rep(1,dimensions_1480),bip1480[,2]))
errors_1480 <- smooth_data_1480 - type_error_1480
errors_1480[is.na(errors_1480)] <- 0
ME_1480 <- apply(errors_1480, 2, mean)
MAE_1480 <- apply(abs(errors_1480), 2, mean)
RMSE_1480 <- sqrt(apply(errors_1480^2, 2, mean))
tot_err_1480 <- rbind(ME_1480, MAE_1480, RMSE_1480)
print(tot_err_1480)

plot(ts(bip1480$BIP001480, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1480, frequency = 52), col='red')
lines(ts(bip1480crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1480, frequency = 52), col='blue')
lines(ts(bip1480SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1480, frequency = 52), col='orange')
lines(ts(bip1480SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP2201#
bip2201 <- select(all_data, X, BIP002201)
bip2201crost <- crost(bip2201[2], h = 5, w = 0.15, init = "naive")
bip2201crost
croston_2201 <- bip2201crost$frc.in
bip2201$cros_smoothed <- croston_2201
bip2201SBA <- crost(bip2201[2], h = 5, type = 'sba', init = 'naive')
bip2201SBA
SBA_2201 <- bip2201SBA$frc.in
bip2201$SBA_smoothed <- SBA_2201
bip2201SBJ <- crost(bip2201[2], h = 5, type = 'sbj', init = 'naive')
bip2201SBJ
SBJ_2201 <- bip2201SBJ$frc.in
bip2201$SBJ_smoothed <- SBJ_2201

type_error_2201 <- cbind(croston_2201, SBA_2201, SBJ_2201)
dimensions_2201 <- dim(type_error_2201)[2]
smooth_data_2201 <- t(tcrossprod(rep(1,dimensions_2201),bip2201[,2]))
errors_2201 <- smooth_data_2201 - type_error_2201
errors_2201[is.na(errors_2201)] <- 0
ME_2201 <- apply(errors_2201, 2, mean)
MAE_2201 <- apply(abs(errors_2201), 2, mean)
RMSE_2201 <- sqrt(apply(errors_2201^2, 2, mean))
tot_err_2201 <- rbind(ME_2201, MAE_2201, RMSE_2201)
print(tot_err_2201)

plot(ts(bip2201$BIP002201, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2201, frequency = 52), col='red')
lines(ts(bip2201crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2201, frequency = 52), col='blue')
lines(ts(bip2201SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2201, frequency = 52), col='orange')
lines(ts(bip2201SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5677#
bip5677 <- select(all_data, X, BIP005677)
bip5677crost <- crost(bip5677[2], h = 5, w = 0.15, init = "naive")
bip5677crost
croston_5677 <- bip5677crost$frc.in
bip5677$cros_smoothed <- croston_5677
bip5677SBA <- crost(bip5677[2], h = 5, type = 'sba', init = 'naive')
bip5677SBA
SBA_5677 <- bip5677SBA$frc.in
bip5677$SBA_smoothed <- SBA_5677
bip5677SBJ <- crost(bip5677[2], h = 5, type = 'sbj', init = 'naive')
bip5677SBJ
SBJ_5677 <- bip5677SBJ$frc.in
bip5677$SBJ_smoothed <- SBJ_5677

type_error_5677 <- cbind(croston_5677, SBA_5677, SBJ_5677)
dimensions_5677 <- dim(type_error_5677)[2]
smooth_data_5677 <- t(tcrossprod(rep(1,dimensions_5677),bip5677[,2]))
errors_5677 <- smooth_data_5677 - type_error_5677
errors_5677[is.na(errors_5677)] <- 0
ME_5677 <- apply(errors_5677, 2, mean)
MAE_5677 <- apply(abs(errors_5677), 2, mean)
RMSE_5677 <- sqrt(apply(errors_5677^2, 2, mean))
tot_err_5677 <- rbind(ME_5677, MAE_5677, RMSE_5677)
print(tot_err_5677)

plot(ts(bip5677$BIP005677, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5677, frequency = 52), col='red')
lines(ts(bip5677crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5677, frequency = 52), col='blue')
lines(ts(bip5677SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5677, frequency = 52), col='orange')
lines(ts(bip5677SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP2459#
bip2459 <- select(all_data, X, BIP002459)
bip2459crost <- crost(bip2459[2], h = 5, w = 0.15, init = "naive")
bip2459crost
croston_2459 <- bip2459crost$frc.in
bip2459$cros_smoothed <- croston_2459
bip2459SBA <- crost(bip2459[2], h = 5, type = 'sba', init = 'naive')
bip2459SBA
SBA_2459 <- bip2459SBA$frc.in
bip2459$SBA_smoothed <- SBA_2459
bip2459SBJ <- crost(bip2459[2], h = 5, type = 'sbj', init = 'naive')
bip2459SBJ
SBJ_2459 <- bip2459SBJ$frc.in
bip2459$SBJ_smoothed <- SBJ_2459

type_error_2459 <- cbind(croston_2459, SBA_2459, SBJ_2459)
dimensions_2459 <- dim(type_error_2459)[2]
smooth_data_2459 <- t(tcrossprod(rep(1,dimensions_2459),bip2459[,2]))
errors_2459 <- smooth_data_2459 - type_error_2459
errors_2459[is.na(errors_2459)] <- 0
ME_2459 <- apply(errors_2459, 2, mean)
MAE_2459 <- apply(abs(errors_2459), 2, mean)
RMSE_2459 <- sqrt(apply(errors_2459^2, 2, mean))
tot_err_2459 <- rbind(ME_2459, MAE_2459, RMSE_2459)
print(tot_err_2459)

plot(ts(bip2459$BIP002459, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2459, frequency = 52), col='red')
lines(ts(bip2459crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2459, frequency = 52), col='blue')
lines(ts(bip2459SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2459, frequency = 52), col='orange')
lines(ts(bip2459SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP1268#
bip1268 <- select(all_data, X, BIP001268)
bip1268crost <- crost(bip1268[2], h = 5, w = 0.15, init = "naive")
bip1268crost
croston_1268 <- bip1268crost$frc.in
bip1268$cros_smoothed <- croston_1268
bip1268SBA <- crost(bip1268[2], h = 5, type = 'sba', init = 'naive')
bip1268SBA
SBA_1268 <- bip1268SBA$frc.in
bip1268$SBA_smoothed <- SBA_1268
bip1268SBJ <- crost(bip1268[2], h = 5, type = 'sbj', init = 'naive')
bip1268SBJ
SBJ_1268 <- bip1268SBJ$frc.in
bip1268$SBJ_smoothed <- SBJ_1268

type_error_1268 <- cbind(croston_1268, SBA_1268, SBJ_1268)
dimensions_1268 <- dim(type_error_1268)[2]
smooth_data_1268 <- t(tcrossprod(rep(1,dimensions_1268),bip1268[,2]))
errors_1268 <- smooth_data_1268 - type_error_1268
errors_1268[is.na(errors_1268)] <- 0
ME_1268 <- apply(errors_1268, 2, mean)
MAE_1268 <- apply(abs(errors_1268), 2, mean)
RMSE_1268 <- sqrt(apply(errors_1268^2, 2, mean))
tot_err_1268 <- rbind(ME_1268, MAE_1268, RMSE_1268)
print(tot_err_1268)

plot(ts(bip1268$BIP001268, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1268, frequency = 52), col='red')
lines(ts(bip1268crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1268, frequency = 52), col='blue')
lines(ts(bip1268SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1268, frequency = 52), col='orange')
lines(ts(bip1268SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP7493#
bip7493 <- select(all_data, X, BIP007493)
bip7493crost <- crost(bip7493[2], h = 5, w = 0.15, init = "naive")
bip7493crost
croston_7493 <- bip7493crost$frc.in
bip7493$cros_smoothed <- croston_7493
bip7493SBA <- crost(bip7493[2], h = 5, type = 'sba', init = 'naive')
bip7493SBA
SBA_7493 <- bip7493SBA$frc.in
bip7493$SBA_smoothed <- SBA_7493
bip7493SBJ <- crost(bip7493[2], h = 5, type = 'sbj', init = 'naive')
bip7493SBJ
SBJ_7493 <- bip7493SBJ$frc.in
bip7493$SBJ_smoothed <- SBJ_7493

type_error_7493 <- cbind(croston_7493, SBA_7493, SBJ_7493)
dimensions_7493 <- dim(type_error_7493)[2]
smooth_data_7493 <- t(tcrossprod(rep(1,dimensions_7493),bip7493[,2]))
errors_7493 <- smooth_data_7493 - type_error_7493
errors_7493[is.na(errors_7493)] <- 0
ME_7493 <- apply(errors_7493, 2, mean)
MAE_7493 <- apply(abs(errors_7493), 2, mean)
RMSE_7493 <- sqrt(apply(errors_7493^2, 2, mean))
tot_err_7493 <- rbind(ME_7493, MAE_7493, RMSE_7493)
print(tot_err_7493)

plot(ts(bip7493$BIP007493, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_7493, frequency = 52), col='red')
lines(ts(bip7493crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_7493, frequency = 52), col='blue')
lines(ts(bip7493SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_7493, frequency = 52), col='orange')
lines(ts(bip7493SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP8649#
bip8649 <- select(all_data, X, BIP008649)
bip8649crost <- crost(bip8649[2], h = 5, w = 0.15, init = "naive")
bip8649crost
croston_8649 <- bip8649crost$frc.in
bip8649$cros_smoothed <- croston_8649
bip8649SBA <- crost(bip8649[2], h = 5, type = 'sba', init = 'naive')
bip8649SBA
SBA_8649 <- bip8649SBA$frc.in
bip8649$SBA_smoothed <- SBA_8649
bip8649SBJ <- crost(bip8649[2], h = 5, type = 'sbj', init = 'naive')
bip8649SBJ
SBJ_8649 <- bip8649SBJ$frc.in
bip8649$SBJ_smoothed <- SBJ_8649

type_error_8649 <- cbind(croston_8649, SBA_8649, SBJ_8649)
dimensions_8649 <- dim(type_error_8649)[2]
smooth_data_8649 <- t(tcrossprod(rep(1,dimensions_8649),bip8649[,2]))
errors_8649 <- smooth_data_8649 - type_error_8649
errors_8649[is.na(errors_8649)] <- 0
ME_8649 <- apply(errors_8649, 2, mean)
MAE_8649 <- apply(abs(errors_8649), 2, mean)
RMSE_8649 <- sqrt(apply(errors_8649^2, 2, mean))
tot_err_8649 <- rbind(ME_8649, MAE_8649, RMSE_8649)
print(tot_err_8649)

plot(ts(bip8649$BIP008649, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_8649, frequency = 52), col='red')
lines(ts(bip8649crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_8649, frequency = 52), col='blue')
lines(ts(bip8649SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_8649, frequency = 52), col='orange')
lines(ts(bip8649SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5601#
bip5601 <- select(all_data, X, BIP005601)
bip5601crost <- crost(bip5601[2], h = 5, w = 0.15, init = "naive")
bip5601crost
croston_5601 <- bip5601crost$frc.in
bip5601$cros_smoothed <- croston_5601
bip5601SBA <- crost(bip5601[2], h = 5, type = 'sba', init = 'naive')
bip5601SBA
SBA_5601 <- bip5601SBA$frc.in
bip5601$SBA_smoothed <- SBA_5601
bip5601SBJ <- crost(bip5601[2], h = 5, type = 'sbj', init = 'naive')
bip5601SBJ
SBJ_5601 <- bip5601SBJ$frc.in
bip5601$SBJ_smoothed <- SBJ_5601

type_error_5601 <- cbind(croston_5601, SBA_5601, SBJ_5601)
dimensions_5601 <- dim(type_error_5601)[2]
smooth_data_5601 <- t(tcrossprod(rep(1,dimensions_5601),bip5601[,2]))
errors_5601 <- smooth_data_5601 - type_error_5601
errors_5601[is.na(errors_5601)] <- 0
ME_5601 <- apply(errors_5601, 2, mean)
MAE_5601 <- apply(abs(errors_5601), 2, mean)
RMSE_5601 <- sqrt(apply(errors_5601^2, 2, mean))
tot_err_5601 <- rbind(ME_5601, MAE_5601, RMSE_5601)
print(tot_err_5601)

plot(ts(bip5601$BIP005601, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5601, frequency = 52), col='red')
lines(ts(bip5601crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5601, frequency = 52), col='blue')
lines(ts(bip5601SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5601, frequency = 52), col='orange')
lines(ts(bip5601SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP3668#
bip3668 <- select(all_data, X, BIP003668)
bip3668crost <- crost(bip3668[2], h = 5, w = 0.15, init = "naive")
bip3668crost
croston_3668 <- bip3668crost$frc.in
bip3668$cros_smoothed <- croston_3668
bip3668SBA <- crost(bip3668[2], h = 5, type = 'sba', init = 'naive')
bip3668SBA
SBA_3668 <- bip3668SBA$frc.in
bip3668$SBA_smoothed <- SBA_3668
bip3668SBJ <- crost(bip3668[2], h = 5, type = 'sbj', init = 'naive')
bip3668SBJ
SBJ_3668 <- bip3668SBJ$frc.in
bip3668$SBJ_smoothed <- SBJ_3668

type_error_3668 <- cbind(croston_3668, SBA_3668, SBJ_3668)
dimensions_3668 <- dim(type_error_3668)[2]
smooth_data_3668 <- t(tcrossprod(rep(1,dimensions_3668),bip3668[,2]))
errors_3668 <- smooth_data_3668 - type_error_3668
errors_3668[is.na(errors_3668)] <- 0
ME_3668 <- apply(errors_3668^2, 2, mean)
MAE_3668 <- apply(abs(errors_3668), 2, mean)
RMSE_3668 <- sqrt(apply(errors_3668^2, 2, mean))
tot_err_3668 <- rbind(ME_3668, MAE_3668, RMSE_3668)
print(tot_err_3668)

?crost

bip3668$err_croston <- errors_3668[,1]
bip3668$err_SBA <- errors_3668[,2]
bip3668$err_SBJ <- errors_3668[,3]

View(bip3668)

plot(ts(bip3668$BIP003668, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_3668, frequency = 52), col='red')
lines(ts(bip3668crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_3668, frequency = 52), col='blue')
lines(ts(bip3668SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_3668, frequency = 52), col='orange')
lines(ts(bip3668SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP3217#
bip3217 <- select(all_data, X, BIP003217)
bip3217crost <- crost(bip3217[2], h = 5, w = 0.15, init = "naive")
bip3217crost
croston_3217 <- bip3217crost$frc.in
bip3217$cros_smoothed <- croston_3217
bip3217SBA <- crost(bip3217[2], h = 5, type = 'sba', init = 'naive')
bip3217SBA
SBA_3217 <- bip3217SBA$frc.in
bip3217$SBA_smoothed <- SBA_3217
bip3217SBJ <- crost(bip3217[2], h = 5, type = 'sbj', init = 'naive')
bip3217SBJ
SBJ_3217 <- bip3217SBJ$frc.in
bip3217$SBJ_smoothed <- SBJ_3217

type_error_3217 <- cbind(croston_3217, SBA_3217, SBJ_3217)
dimensions_3217 <- dim(type_error_3217)[2]
smooth_data_3217 <- t(tcrossprod(rep(1,dimensions_3217),bip3217[,2]))
errors_3217 <- smooth_data_3217 - type_error_3217
errors_3217[is.na(errors_3217)] <- 0
ME_3217 <- apply(errors_3217, 2, mean)
MAE_3217 <- apply(abs(errors_3217), 2, mean)
RMSE_3217 <- sqrt(apply(errors_3217^2, 2, mean))
tot_err_3217 <- rbind(ME_3217, MAE_3217, RMSE_3217)
print(tot_err_3217)

plot(ts(bip3217$BIP003217, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_3217, frequency = 52), col='red')
lines(ts(bip3217crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_3217, frequency = 52), col='blue')
lines(ts(bip3217SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_3217, frequency = 52), col='orange')
lines(ts(bip3217SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5556#
bip5556 <- select(all_data, X, BIP005556)
bip5556crost <- crost(bip5556[2], h = 5, w = 0.15, init = "naive")
bip5556crost
croston_5556 <- bip5556crost$frc.in
bip5556$cros_smoothed <- croston_5556
bip5556SBA <- crost(bip5556[2], h = 5, type = 'sba', init = 'naive')
bip5556SBA
SBA_5556 <- bip5556SBA$frc.in
bip5556$SBA_smoothed <- SBA_5556
bip5556SBJ <- crost(bip5556[2], h = 5, type = 'sbj', init = 'naive')
bip5556SBJ
SBJ_5556 <- bip5556SBJ$frc.in
bip5556$SBJ_smoothed <- SBJ_5556

type_error_5556 <- cbind(croston_5556, SBA_5556, SBJ_5556)
dimensions_5556 <- dim(type_error_5556)[2]
smooth_data_5556 <- t(tcrossprod(rep(1,dimensions_5556),bip5556[,2]))
errors_5556 <- smooth_data_5556 - type_error_5556
errors_5556[is.na(errors_5556)] <- 0
ME_5556 <- apply(errors_5556, 2, mean)
MAE_5556 <- apply(abs(errors_5556), 2, mean)
RMSE_5556 <- sqrt(apply(errors_5556^2, 2, mean))
tot_err_5556 <- rbind(ME_5556, MAE_5556, RMSE_5556)
print(tot_err_5556)

plot(ts(bip5556$BIP005556, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5556, frequency = 52), col='red')
lines(ts(bip5556crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5556, frequency = 52), col='blue')
lines(ts(bip5556SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5556, frequency = 52), col='orange')
lines(ts(bip5556SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP7666#
bip7666 <- select(all_data, X, BIP007666)
bip7666crost <- crost(bip7666[2], h = 5, w = 0.15, init = "naive")
bip7666crost
croston_7666 <- bip7666crost$frc.in
bip7666$cros_smoothed <- croston_7666
bip7666SBA <- crost(bip7666[2], h = 5, type = 'sba', init = 'naive')
bip7666SBA
SBA_7666 <- bip7666SBA$frc.in
bip7666$SBA_smoothed <- SBA_7666
bip7666SBJ <- crost(bip7666[2], h = 5, type = 'sbj', init = 'naive')
bip7666SBJ
SBJ_7666 <- bip7666SBJ$frc.in
bip7666$SBJ_smoothed <- SBJ_7666

type_error_7666 <- cbind(croston_7666, SBA_7666, SBJ_7666)
dimensions_7666 <- dim(type_error_7666)[2]
smooth_data_7666 <- t(tcrossprod(rep(1,dimensions_7666),bip7666[,2]))
errors_7666 <- smooth_data_7666 - type_error_7666
errors_7666[is.na(errors_7666)] <- 0
ME_7666 <- apply(errors_7666, 2, mean)
MAE_7666 <- apply(abs(errors_7666), 2, mean)
RMSE_7666 <- sqrt(apply(errors_7666^2, 2, mean))
tot_err_7666 <- rbind(ME_7666, MAE_7666, RMSE_7666)
print(tot_err_7666)

plot(ts(bip7666$BIP007666, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_7666, frequency = 52), col='red')
lines(ts(bip7666crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_7666, frequency = 52), col='blue')
lines(ts(bip7666SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_7666, frequency = 52), col='orange')
lines(ts(bip7666SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP3105#
bip3105 <- select(all_data, X, BIP003105)
bip3105crost <- crost(bip3105[2], h = 5, w = 0.15, init = "naive")
bip3105crost
croston_3105 <- bip3105crost$frc.in
bip3105$cros_smoothed <- croston_3105
bip3105SBA <- crost(bip3105[2], h = 5, type = 'sba', init = 'naive')
bip3105SBA
SBA_3105 <- bip3105SBA$frc.in
bip3105$SBA_smoothed <- SBA_3105
bip3105SBJ <- crost(bip3105[2], h = 5, type = 'sbj', init = 'naive')
bip3105SBJ
SBJ_3105 <- bip3105SBJ$frc.in
bip3105$SBJ_smoothed <- SBJ_3105

type_error_3105 <- cbind(croston_3105, SBA_3105, SBJ_3105)
dimensions_3105 <- dim(type_error_3105)[2]
smooth_data_3105 <- t(tcrossprod(rep(1,dimensions_3105),bip3105[,2]))
errors_3105 <- smooth_data_3105 - type_error_3105
errors_3105[is.na(errors_3105)] <- 0
ME_3105 <- apply(errors_3105, 2, mean)
MAE_3105 <- apply(abs(errors_3105), 2, mean)
RMSE_3105 <- sqrt(apply(errors_3105^2, 2, mean))
tot_err_3105 <- rbind(ME_3105, MAE_3105, RMSE_3105)
print(tot_err_3105)

plot(ts(bip3105$BIP003105, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_3105, frequency = 52), col='red')
lines(ts(bip3105crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_3105, frequency = 52), col='blue')
lines(ts(bip3105SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_3105, frequency = 52), col='orange')
lines(ts(bip3105SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP4395#
bip4395 <- select(all_data, X, BIP004395)
bip4395crost <- crost(bip4395[2], h = 5, w = 0.15, init = "naive")
bip4395crost
croston_4395 <- bip4395crost$frc.in
bip4395$cros_smoothed <- croston_4395
bip4395SBA <- crost(bip4395[2], h = 5, type = 'sba', init = 'naive')
bip4395SBA
SBA_4395 <- bip4395SBA$frc.in
bip4395$SBA_smoothed <- SBA_4395
bip4395SBJ <- crost(bip4395[2], h = 5, type = 'sbj', init = 'naive')
bip4395SBJ
SBJ_4395 <- bip4395SBJ$frc.in
bip4395$SBJ_smoothed <- SBJ_4395

type_error_4395 <- cbind(croston_4395, SBA_4395, SBJ_4395)
dimensions_4395 <- dim(type_error_4395)[2]
smooth_data_4395 <- t(tcrossprod(rep(1,dimensions_4395),bip4395[,2]))
errors_4395 <- smooth_data_4395 - type_error_4395
errors_4395[is.na(errors_4395)] <- 0
ME_4395 <- apply(errors_4395, 2, mean)
MAE_4395 <- apply(abs(errors_4395), 2, mean)
RMSE_4395 <- sqrt(apply(errors_4395^2, 2, mean))
tot_err_4395 <- rbind(ME_4395, MAE_4395, RMSE_4395)
print(tot_err_4395)

plot(ts(bip4395$BIP004395, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_4395, frequency = 52), col='red')
lines(ts(bip4395crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_4395, frequency = 52), col='blue')
lines(ts(bip4395SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_4395, frequency = 52), col='orange')
lines(ts(bip4395SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5900#
bip5900 <- select(all_data, X, BIP005900)
bip5900crost <- crost(bip5900[2], h = 5, w = 0.15, init = "naive")
bip5900crost
croston_5900 <- bip5900crost$frc.in
bip5900$cros_smoothed <- croston_5900
bip5900SBA <- crost(bip5900[2], h = 5, type = 'sba', init = 'naive')
bip5900SBA
SBA_5900 <- bip5900SBA$frc.in
bip5900$SBA_smoothed <- SBA_5900
bip5900SBJ <- crost(bip5900[2], h = 5, type = 'sbj', init = 'naive')
bip5900SBJ
SBJ_5900 <- bip5900SBJ$frc.in
bip5900$SBJ_smoothed <- SBJ_5900

type_error_5900 <- cbind(croston_5900, SBA_5900, SBJ_5900)
dimensions_5900 <- dim(type_error_5900)[2]
smooth_data_5900 <- t(tcrossprod(rep(1,dimensions_5900),bip5900[,2]))
errors_5900 <- smooth_data_5900 - type_error_5900
errors_5900[is.na(errors_5900)] <- 0
ME_5900 <- apply(errors_5900, 2, mean)
MAE_5900 <- apply(abs(errors_5900), 2, mean)
RMSE_5900 <- sqrt(apply(errors_5900^2, 2, mean))
tot_err_5900 <- rbind(ME_5900, MAE_5900, RMSE_5900)
print(tot_err_5900)

plot(ts(bip5900$BIP005900, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5900, frequency = 52), col='red')
lines(ts(bip5900crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5900, frequency = 52), col='blue')
lines(ts(bip5900SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5900, frequency = 52), col='orange')
lines(ts(bip5900SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP8822#
bip8822 <- select(all_data, X, BIP008822)
bip8822crost <- crost(bip8822[2], h = 5, w = 0.15, init = "naive")
bip8822crost
croston_8822 <- bip8822crost$frc.in
bip8822$cros_smoothed <- croston_8822
bip8822SBA <- crost(bip8822[2], h = 5, type = 'sba', init = 'naive')
bip8822SBA
SBA_8822 <- bip8822SBA$frc.in
bip8822$SBA_smoothed <- SBA_8822
bip8822SBJ <- crost(bip8822[2], h = 5, type = 'sbj', init = 'naive')
bip8822SBJ
SBJ_8822 <- bip8822SBJ$frc.in
bip8822$SBJ_smoothed <- SBJ_8822

type_error_8822 <- cbind(croston_8822, SBA_8822, SBJ_8822)
dimensions_8822 <- dim(type_error_8822)[2]
smooth_data_8822 <- t(tcrossprod(rep(1,dimensions_8822),bip8822[,2]))
errors_8822 <- smooth_data_8822 - type_error_8822
errors_8822[is.na(errors_8822)] <- 0
ME_8822 <- apply(errors_8822, 2, mean)
MAE_8822 <- apply(abs(errors_8822), 2, mean)
RMSE_8822 <- sqrt(apply(errors_8822^2, 2, mean))
tot_err_8822 <- rbind(ME_8822, MAE_8822, RMSE_8822)
print(tot_err_8822)

plot(ts(bip8822$BIP008822, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_8822, frequency = 52), col='red')
lines(ts(bip8822crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_8822, frequency = 52), col='blue')
lines(ts(bip8822SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_8822, frequency = 52), col='orange')
lines(ts(bip8822SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP4396#
bip4396 <- select(all_data, X, BIP004396)
bip4396crost <- crost(bip4396[2], h = 5, w = 0.15, init = "naive")
bip4396crost
croston_4396 <- bip4396crost$frc.in
bip4396$cros_smoothed <- croston_4396
bip4396SBA <- crost(bip4396[2], h = 5, type = 'sba', init = 'naive')
bip4396SBA
SBA_4396 <- bip4396SBA$frc.in
bip4396$SBA_smoothed <- SBA_4396
bip4396SBJ <- crost(bip4396[2], h = 5, type = 'sbj', init = 'naive')
bip4396SBJ
SBJ_4396 <- bip4396SBJ$frc.in
bip4396$SBJ_smoothed <- SBJ_4396

type_error_4396 <- cbind(croston_4396, SBA_4396, SBJ_4396)
dimensions_4396 <- dim(type_error_4396)[2]
smooth_data_4396 <- t(tcrossprod(rep(1,dimensions_4396),bip4396[,2]))
errors_4396 <- smooth_data_4396 - type_error_4396
errors_4396[is.na(errors_4396)] <- 0
ME_4396 <- apply(errors_4396, 2, mean)
MAE_4396 <- apply(abs(errors_4396), 2, mean)
RMSE_4396 <- sqrt(apply(errors_4396^2, 2, mean))
tot_err_4396 <- rbind(ME_4396, MAE_4396, RMSE_4396)
print(tot_err_4396)

plot(ts(bip4396$BIP004396, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_4396, frequency = 52), col='red')
lines(ts(bip4396crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_4396, frequency = 52), col='blue')
lines(ts(bip4396SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_4396, frequency = 52), col='orange')
lines(ts(bip4396SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP9106#
bip9106 <- select(all_data, X, BIP009106)
bip9106crost <- crost(bip9106[2], h = 5, w = 0.15, init = "naive")
bip9106crost
croston_9106 <- bip9106crost$frc.in
bip9106$cros_smoothed <- croston_9106
bip9106SBA <- crost(bip9106[2], h = 5, type = 'sba', init = 'naive')
bip9106SBA
SBA_9106 <- bip9106SBA$frc.in
bip9106$SBA_smoothed <- SBA_9106
bip9106SBJ <- crost(bip9106[2], h = 5, type = 'sbj', init = 'naive')
bip9106SBJ
SBJ_9106 <- bip9106SBJ$frc.in
bip9106$SBJ_smoothed <- SBJ_9106

type_error_9106 <- cbind(croston_9106, SBA_9106, SBJ_9106)
dimensions_9106 <- dim(type_error_9106)[2]
smooth_data_9106 <- t(tcrossprod(rep(1,dimensions_9106),bip9106[,2]))
errors_9106 <- smooth_data_9106 - type_error_9106
errors_9106[is.na(errors_9106)] <- 0
ME_9106 <- apply(errors_9106, 2, mean)
MAE_9106 <- apply(abs(errors_9106), 2, mean)
RMSE_9106 <- sqrt(apply(errors_9106^2, 2, mean))
tot_err_9106 <- rbind(ME_9106, MAE_9106, RMSE_9106)
print(tot_err_9106)

plot(ts(bip9106$BIP009106, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_9106, frequency = 52), col='red')
lines(ts(bip9106crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_9106, frequency = 52), col='blue')
lines(ts(bip9106SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_9106, frequency = 52), col='orange')
lines(ts(bip9106SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP5464#
bip5464 <- select(all_data, X, BIP005464)
bip5464crost <- crost(bip5464[2], h = 5, w = 0.15, init = "naive")
bip5464crost
croston_5464 <- bip5464crost$frc.in
bip5464$cros_smoothed <- croston_5464
bip5464SBA <- crost(bip5464[2], h = 5, type = 'sba', init = 'naive')
bip5464SBA
SBA_5464 <- bip5464SBA$frc.in
bip5464$SBA_smoothed <- SBA_5464
bip5464SBJ <- crost(bip5464[2], h = 5, type = 'sbj', init = 'naive')
bip5464SBJ
SBJ_5464 <- bip5464SBJ$frc.in
bip5464$SBJ_smoothed <- SBJ_5464

type_error_5464 <- cbind(croston_5464, SBA_5464, SBJ_5464)
dimensions_5464 <- dim(type_error_5464)[2]
smooth_data_5464 <- t(tcrossprod(rep(1,dimensions_5464),bip5464[,2]))
errors_5464 <- smooth_data_5464 - type_error_5464
errors_5464[is.na(errors_5464)] <- 0
ME_5464 <- apply(errors_5464, 2, mean)
MAE_5464 <- apply(abs(errors_5464), 2, mean)
RMSE_5464 <- sqrt(apply(errors_5464^2, 2, mean))
tot_err_5464 <- rbind(ME_5464, MAE_5464, RMSE_5464)
print(tot_err_5464)

plot(ts(bip5464$BIP005464, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_5464, frequency = 52), col='red')
lines(ts(bip5464crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_5464, frequency = 52), col='blue')
lines(ts(bip5464SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_5464, frequency = 52), col='orange')
lines(ts(bip5464SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP1877#
bip1877 <- select(all_data, X, BIP001877)
bip1877crost <- crost(bip1877[2], h = 5, w = 0.15, init = "naive")
bip1877crost
croston_1877 <- bip1877crost$frc.in
bip1877$cros_smoothed <- croston_1877
bip1877SBA <- crost(bip1877[2], h = 5, type = 'sba', init = 'naive')
bip1877SBA
SBA_1877 <- bip1877SBA$frc.in
bip1877$SBA_smoothed <- SBA_1877
bip1877SBJ <- crost(bip1877[2], h = 5, type = 'sbj', init = 'naive')
bip1877SBJ
SBJ_1877 <- bip1877SBJ$frc.in
bip1877$SBJ_smoothed <- SBJ_1877

type_error_1877 <- cbind(croston_1877, SBA_1877, SBJ_1877)
dimensions_1877 <- dim(type_error_1877)[2]
smooth_data_1877 <- t(tcrossprod(rep(1,dimensions_1877),bip1877[,2]))
errors_1877 <- smooth_data_1877 - type_error_1877
errors_1877[is.na(errors_1877)] <- 0
ME_1877 <- apply(errors_1877, 2, mean)
MAE_1877 <- apply(abs(errors_1877), 2, mean)
RMSE_1877 <- sqrt(apply(errors_1877^2, 2, mean))
tot_err_1877 <- rbind(ME_1877, MAE_1877, RMSE_1877)
print(tot_err_1877)

plot(ts(bip1877$BIP001877, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_1877, frequency = 52), col='red')
lines(ts(bip1877crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_1877, frequency = 52), col='blue')
lines(ts(bip1877SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_1877, frequency = 52), col='orange')
lines(ts(bip1877SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP2894#
bip2894 <- select(all_data, X, BIP002894)
bip2894crost <- crost(bip2894[2], h = 5, w = 0.15, init = "naive")
bip2894crost
croston_2894 <- bip2894crost$frc.in
bip2894$cros_smoothed <- croston_2894
bip2894SBA <- crost(bip2894[2], h = 5, type = 'sba', init = 'naive')
bip2894SBA
SBA_2894 <- bip2894SBA$frc.in
bip2894$SBA_smoothed <- SBA_2894
bip2894SBJ <- crost(bip2894[2], h = 5, type = 'sbj', init = 'naive')
bip2894SBJ
SBJ_2894 <- bip2894SBJ$frc.in
bip2894$SBJ_smoothed <- SBJ_2894

type_error_2894 <- cbind(croston_2894, SBA_2894, SBJ_2894)
dimensions_2894 <- dim(type_error_2894)[2]
smooth_data_2894 <- t(tcrossprod(rep(1,dimensions_2894),bip2894[,2]))
errors_2894 <- smooth_data_2894 - type_error_2894
errors_2894[is.na(errors_2894)] <- 0
ME_2894 <- apply(errors_2894, 2, mean)
MAE_2894 <- apply(abs(errors_2894), 2, mean)
RMSE_2894 <- sqrt(apply(errors_2894^2, 2, mean))
tot_err_2894 <- rbind(ME_2894, MAE_2894, RMSE_2894)
print(tot_err_2894)

plot(ts(bip2894$BIP002894, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2894, frequency = 52), col='red')
lines(ts(bip2894crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2894, frequency = 52), col='blue')
lines(ts(bip2894SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2894, frequency = 52), col='orange')
lines(ts(bip2894SBJ$frc.out, start = c(1, 1.69)), col='purple')

#Croston model for item BIP2899#
bip2899 <- select(all_data, X, BIP002899)
bip2899crost <- crost(bip2899[2], h = 5, w = 0.15, init = "naive")
bip2899crost
croston_2899 <- bip2899crost$frc.in
bip2899$cros_smoothed <- croston_2899
bip2899SBA <- crost(bip2899[2], h = 5, type = 'sba', init = 'naive')
bip2899SBA
SBA_2899 <- bip2899SBA$frc.in
bip2899$SBA_smoothed <- SBA_2899
bip2899SBJ <- crost(bip2899[2], h = 5, type = 'sbj', init = 'naive')
bip2899SBJ
SBJ_2899 <- bip2899SBJ$frc.in
bip2899$SBJ_smoothed <- SBJ_2899

type_error_2899 <- cbind(croston_2899, SBA_2899, SBJ_2899)
dimensions_2899 <- dim(type_error_2899)[2]
smooth_data_2899 <- t(tcrossprod(rep(1,dimensions_2899),bip2899[,2]))
errors_2899 <- smooth_data_2899 - type_error_2899
errors_2899[is.na(errors_2899)] <- 0
ME_2899 <- apply(errors_2899, 2, mean)
MAE_2899 <- apply(abs(errors_2899), 2, mean)
RMSE_2899 <- sqrt(apply(errors_2899^2, 2, mean))
tot_err_2899 <- rbind(ME_2899, MAE_2899, RMSE_2899)
print(tot_err_2899)

plot(ts(bip2899$BIP002899, frequency = 52), type = 'b', xlim = c(1, 1.8))
lines(ts(croston_2899, frequency = 52), col='red')
lines(ts(bip2899crost$frc.out, start=c(1,1.69)), col='green')

lines(ts(SBA_2899, frequency = 52), col='blue')
lines(ts(bip2899SBA$frc.out, start = c(1,1.69)), col='yellow')

lines(ts(SBJ_2899, frequency = 52), col='orange')
lines(ts(bip2899SBJ$frc.out, start = c(1, 1.69)), col='purple')





