install.packages("ggplot2")
library(forecast)
library(tsintermittent)
library(ggplot2)

all_data <- read.csv("C:/Users/2506/Desktop/TESIS/DOCUMENTOS TESIS/Capitulos/R/Tesis/AllData.csv", header = TRUE)
bip1271 <- all_data[,1:2]
bip1271crost <- crost(bip1271[2], h = 5)
bip1271crost
bip1271$cros_smoothed <- bip1271crost$frc.in

plot.ts(bip1271$BIP001271, xlim = c(0, 40))
lines(bip1271crost$frc.in, col="red")
lines(bip1271crost$frc.out, col = "blue")

bip1271crost



qplot(X, BIP001271, data = bip1271, geom = c("point", "line"), aes(group = 1)) + 
  geom_hline(mapping = (aes(group = 1)), bip1271$cros_smoothed)


ggplot(bip1271, aes(x = X, y = BIP001271, group = 2))+
  geom_line(size=1, colour = "black") 
geom_hline

