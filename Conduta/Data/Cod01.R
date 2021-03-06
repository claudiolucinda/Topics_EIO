################################################
# Modelo Conduta -- Non-Nested Models
# Claudio R. Lucinda
# 2020
# FEA-USP
################################################

library(tidyverse)
library(AER)
library(readxl)
library(plm)

setwd("G:/Meu Drive/Aulas/GV/Curso de OI - P�s/Mini Curso USP/Topics_EIO/Conduta/Data")

# Dados da prova final de OI Emp�rica I

data<-read_xls("Cereal_Data.xls")

data2<-data[1:50,]

s0<-data$`Mkt Share`[51]

data2$brand<-substr(data2$Name,start=1,stop=2)

#Respostas dos itens da prova

data2$meanu<-log(data2$`Mkt Share`)-log(s0)

# OLS sem Brand FE

model01<-lm(meanu~`Avg Shelf Price`+Cals+Fat+Sugar+factor(Sgmnt), data=data2)
summary(model01)

# OLS com brand FE

model02<-lm(meanu~`Avg Shelf Price`+Cals+Fat+Sugar+factor(Sgmnt)+factor(brand), data=data2)
summary(model02)
coeftest(model02,vcov.=sandwich)

# IV. Eu n�o disse quais IV vcs tinham que usar na prova, isso era de prop�sito
# para voc�s lembrarem das minhas aulas.
# Aqui vamos criar os instrumentos BLP -- Poder�amos criar os BST, ou os GH.
# Fica um exerc�cio para o lar.

data2<-data2 %>%
  group_by(brand) %>%
  mutate(tot_Cals=sum(Cals)) %>%
  mutate(BLP_Cals=tot_Cals-Cals)

data2<-data2 %>%
  group_by(brand) %>%
  mutate(tot_Fat=sum(Fat)) %>%
  mutate(BLP_Fat=tot_Fat-Fat)

data2<-data2 %>%
  group_by(brand) %>%
  mutate(tot_Sugar=sum(Sugar)) %>%
  mutate(BLP_Sugar=tot_Sugar-Sugar)

data2$temp<-1

data2<-data2 %>%
  group_by(temp) %>%
  mutate(o_tot_Cals=sum(Cals)) %>%
  mutate(BLP2_Cals=o_tot_Cals-tot_Cals)

data2<-data2 %>%
  group_by(temp) %>%
  mutate(o_tot_Fat=sum(Fat)) %>%
  mutate(BLP2_Fat=o_tot_Fat-tot_Fat)

data2<-data2 %>%
  group_by(temp) %>%
  mutate(o_tot_Sugar=sum(Sugar)) %>%
  mutate(BLP2_Sugar=o_tot_Sugar-tot_Sugar)


######################################
# IV Regression
######################################

model03<-ivreg(meanu~`Avg Shelf Price`+Cals+Fat+Sugar+factor(Sgmnt) | 
                 Cals+Fat+Sugar+factor(Sgmnt)+BLP_Cals+BLP_Fat+BLP_Sugar +
                 BLP2_Cals+BLP2_Fat+BLP2_Sugar, data=data2)
summary(model03, vcov = sandwich, diagnostics = TRUE)

model04<-ivreg(meanu~`Avg Shelf Price`+Cals+Fat+Sugar+factor(Sgmnt)+factor(brand) | 
                 Cals+Fat+Sugar+factor(Sgmnt)+factor(brand)+BLP_Cals+BLP_Fat+BLP_Sugar +
                 BLP2_Cals+BLP2_Fat+BLP2_Sugar, data=data2)
summary(model04, diagnostics = TRUE)

# Efeitos Marginais - Vai ser importante pro nosso exerc�cio

sharevec<-as.numeric(data2$`Mkt Share`)/100
pricevec<-as.numeric(data2$`Avg Shelf Price`)
beta_coeff<-model04$coefficients["`Avg Shelf Price`"]

elast_mat<-matrix(-beta_coeff*sharevec*pricevec,nrow=length(sharevec), ncol=length(sharevec), byrow=TRUE)
elast_mat<-elast_mat + diag(beta_coeff*pricevec)


mgeff_mat<--beta_coeff*(sharevec %*% t(sharevec))
mgeff_mat<- mgeff_mat+diag(beta_coeff*sharevec)


dum_brand<-model.matrix(~brand-1, data=data2)
omega<-dum_brand %*% t(dum_brand)

###############################################
# Calculando os custos marginais
###############################################
# Betrand Multiproduto
###############################################

m0<-(-solve(t(elast_mat)*omega) %*% sharevec) *(1/sharevec)
cmg0<-(1-m0)*pricevec

###############################################
# Cartel
###############################################

omega_cartel<-matrix(1,nrow=50,ncol=50)
m_c<-(-solve(t(elast_mat)*omega_cartel) %*% sharevec) *(1/sharevec)
cmg_c<-(1-m_c)*pricevec

################################################
# Apresentando Graficamente
################################################
# Custos marginais
################################################

mat<-t(cbind(cmg0,cmg_c))
rownames(mat)<-c("Bertrand","Cartel")
barplot(mat, beside=TRUE, legend.text = c("Bertrand", "Cartel"), 
        args.legend = list(x="topleft", bty="n", inset=c(-0.1,0)) )

mat<-t(cbind(m0,m_c))
rownames(mat)<-c("Bertrand","Cartel")
barplot(mat, beside=TRUE, legend.text = c("Bertrand", "Cartel"), 
        args.legend = list(x="topleft", bty="n", inset=c(-0.1,0)) )



