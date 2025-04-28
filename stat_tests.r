library(ggplot2)
library(tidyverse)

unemployment <- read.csv("data/2021-2022unemployment.csv")

employed <- c(unemployment$Employed)
unemployed <- c(unemployment$Unemployed)

e20 <- employed[10]
u20 <- unemployed[10]
e25 <- employed[15]
u25 <- unemployed[15]

tot20 <- e20 + u20
tot25 <- e25 + u25

etot <- e20 + e25
utot <- u20 + u25
tot <- etot + utot

erateest <- etot/tot
urateest <- utot/tot

e20est <- erateest * tot20 
u20est <- urateest * tot20
e25est <- erateest * tot25
u25est <- urateest * tot25

ts <- ((e20 - e20est)/e20est) + ((u20 - u20est)/u20est) + ((e25 - e25est)/e25est) + ((u25 - u25est)/u25est)

p <- pchisq(ts, 1, lower.tail=FALSE)