#############################################
# Codes for Production Function Estimation
# Claudio R. Lucinda
# University of São Paulo
# 2020
##############################################

#install.packages("AER")
#install.ppackages("tidyverse")
#install.packages("estprod")
#install.packages("foreign")
#install.packages("plm")

library("AER")
library("tidyverse")
library("estprod")
library("foreign")
library("plm")

data<-read.dta("sample.dta")

# OLS sem clustered Std. Errors
model01<-plm(lnva~lnk+lnb+lnw, data=data, model="pooling", index=c("plantid","year"))
# Erros-Padrão "normais"
summary(model01)

# Modelo anterior, com clustered SE por plantid -- igual ao Stata
# Dá uma baixada boa nos Erros Padrão
# E tá igual ao Stata
coeftest(model01,vcov=vcovHC(model01, type="sss", cluster="group"))


# FE com clustered SE
# Pergunta ao leitor. O que quer dizer a constante nos FE no Stata?

model02<-plm(lnva~lnk+lnb+lnw, data=data, model="within", index=c("plantid","year"))
coeftest(model02,vcov=vcovHC(model02, type="sss", cluster="group"))

# Colocando FE de ano
# OLS sem clustered Std. Errors
model03<-plm(lnva~lnk+lnb+lnw+factor(year), data=data, model="pooling", index=c("plantid","year"))
coeftest(model03,vcov=vcovHC(model03, type="sss", cluster="group"))

model04<-plm(lnva~lnk+lnb+lnw+factor(year), data=data, model="within", index=c("plantid","year"))
coeftest(model04,vcov=vcovHC(model04, type="sss", cluster="group"))


# Balanced Data
data_bal<-make.pbalanced(data,balance.type = "shared.individuals", index=c("plantid","year"))

# Comparando as amostras
summary(data_bal)
summary(data)

# Mesma coisa de antes, só com o painel balanceado
model05<-plm(lnva~lnk+lnb+lnw, data=data_bal, model="pooling", index=c("plantid","year"))
coeftest(model05,vcov=vcovHC(model05, type="sss", cluster="group"))

model06<-plm(lnva~lnk+lnb+lnw, data=data_bal, model="within", index=c("plantid","year"))
coeftest(model06,vcov=vcovHC(model06, type="sss", cluster="group"))

model07<-plm(lnva~lnk+lnb+lnw+factor(year), data=data_bal, model="pooling", index=c("plantid","year"))
coeftest(model07,vcov=vcovHC(model07, type="sss", cluster="group"))

model08<-plm(lnva~lnk+lnb+lnw+factor(year), data=data_bal, model="within", index=c("plantid","year"))
coeftest(model08,vcov=vcovHC(model08, type="sss", cluster="group"))


############################################
# Dynamic Panel Data
############################################
# Código pra recuperar os coeficientes estruturais
# Puxado de Millo (2012, JAE, Online appendix)
source("comfactest.R")

# Arellano-Bond

m1<-pgmm(lnva~lag(lnva,1)+lag(lnk,0:1)+lag(lnw,0:1)+lag(lnb,0:1) | 
           lag(lnva,2:99) + lag(lnw,2:99) +lag(lnb, 2:99), data = data, index=c("plantid","year"),
         effects="twoways", fsm = "G")
summary(m1, robust=TRUE)
comfactest(m1)

# Blundell-Bond
m2<-pgmm(lnva~lag(lnva,1)+lag(lnk,0:1)+lag(lnw,0:1)+lag(lnb,0:1) | 
           lag(lnva,2:99) + lag(lnw,2:99) +lag(lnb, 2:99), data = data, index=c("plantid","year"),
         effects="twoways", fsm = "G", transformation = "ld")
summary(m2, robust=TRUE)
comfactest(m2, k=15)

##########################################################
# Structural Estimation (OP, ACF, LP and other stuff)
##########################################################
rm(data_2)
data_2<-data %>%
  group_by(plantid) %>%
  mutate(count=n())

data_2$survivor<-(data_2$count==10)
data_2$has95<-(data_2$year==1991)
data_2<-data_2 %>%
  group_by(plantid) %>%
  mutate(has955=max(has95)) 

data_2<-data_2 %>%
  arrange(plantid,year) %>%
  mutate(has_gaps=year-1==dplyr::lag(year,order_by=plantid)) %>%
  mutate(nobs=seq(n()))

data_2$hass_gaps<-(data_2$has_gaps==FALSE) & data_2$nobs>1

data_2$exit<-(data_2$survivor==0) & (data_2$has955==0) & (data_2$hass_gaps==0) & (data_2$count==data_2$nobs)

data_3<-data_2[complete.cases(data_2),]

mod_op<-olley_pakes(data_3, lnva~lnw+lnb | lnk | lnm , exit=~exit,
                    id="plantid", time="year", bootstrap =TRUE)
summary(mod_op)

mod_wo<-wooldridge(data_3, lnva~lnw+lnb | lnk | lnm , exit=~exit,
                   id="plantid", time="year", bootstrap =TRUE)
summary(mod_wo)

mod_lp<-levinsohn_petrin(data_3, lnva~lnw+lnb | lnk | lnm , exit=~exit,
                   id="plantid", time="year", bootstrap =TRUE)
summary(mod_lp)

#ACF -- Package "prodest"
#Next year
