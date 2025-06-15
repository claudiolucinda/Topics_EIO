######################################
# C?digo para implementar GPV (2000)
# Claudio R. Lucinda
# 26/06/2020
# USP
#######################################


library(tidyverse)
library(ggplot2)

rm(list=ls())

setwd("G:\\Meu Drive\\Aulas\\GV\\Curso de OI - Pós\\Mini Curso USP\\Topics_EIO\\Auctions\\Data")

data<-read_csv("PS3Data.csv")

data <- data %>% 
  pivot_longer(cols = c("Bidder 1", "Bidder 2"), names_to = "bid_code", values_to = "bidder")

data <- data %>%
  arrange(bidder)

data <- data %>%
  mutate(cdf=seq(n())/1000)

dens<-density(data$bidder,kernel="epanechnikov", n=1000)
teste<-approx(dens$x, dens$y, xout=data$bidder)

data$pdf<-teste$y

data<- data %>%
  mutate(valuation=bidder+cdf/pdf) %>%
  mutate(check=valuation-((1-cdf)/pdf))