##########################################
# Código para estimação de Sistemas 
# Neoclássicos de Demanda
# Claudio R. Lucinda
# 2020
###########################################
# 1) Pré-Rquisitos
###########################################
#install.packages("systemfit")
install.packages("micEconAids")
library("systemfit")
library("micEconAids")

data<-read.delim("EVIEWS.txt", header=TRUE, sep="\t")
data$valtot<-data$V_PEIXE+data$V_CARNE+data$V_PORCO+data$V_FRANGO

data$S_PEIXE<-data$V_PEIXE/data$valtot
data$S_CARNE<-data$V_CARNE/data$valtot
data$S_PORCO<-data$V_PORCO/data$valtot
data$S_FRANGO<-data$V_FRANGO/data$valtot

data$L_P_PEIXE<-log(data$P_PEIXE)
data$L_P_CARNE<-log(data$P_CARNE)
data$L_P_PORCO<-log(data$P_PORCO)
data$L_P_FRANGO<-log(data$P_FRANGO)
data$L_valtot<-log(data$valtot)
data$L_S_PEIXE<-log(data$S_PEIXE)
data$L_S_CARNE<-log(data$S_CARNE)
data$L_S_PORCO<-log(data$S_PORCO)
data$L_S_FRANGO<-log(data$S_FRANGO)
data$L_S_PEIXE<-ifelse(is.infinite(-data$L_S_PEIXE), NA, data$L_S_PEIXE)
data$L_S_CARNE<-ifelse(is.infinite(-data$L_S_CARNE), NA, data$L_S_CARNE)
data$L_S_PORCO<-ifelse(is.infinite(-data$L_S_PORCO), NA, data$L_S_PORCO)
data$L_S_FRANGO<-ifelse(is.infinite(-data$L_S_FRANGO), NA, data$L_S_FRANGO)


###########################################
# Modelo Duplo Log
###########################################
peixe.formula<-L_S_PEIXE ~ a1 + (b1-1)*L_valtot + (c11+1)*L_P_PEIXE + c12*L_P_CARNE + c13*L_P_PORCO + c14*L_P_FRANGO
carne.formula<-L_S_CARNE ~ a2 + (b2-1)*L_valtot + c21*L_P_PEIXE + (c22+1)*L_P_CARNE + c23*L_P_PORCO + c24*L_P_FRANGO
porco.formula<-L_S_PORCO ~ a3 + (b3-1)*L_valtot + c31*L_P_PEIXE + c32*L_P_CARNE + (c33+1)*L_P_PORCO + c34*L_P_FRANGO
frango.formula<-L_S_CARNE ~ a4 + (b4-1)*L_valtot + c41*L_P_PEIXE + c42*L_P_CARNE + c43*L_P_PORCO + (c44+1)*L_P_FRANGO

labels.list<-list("Peixe","Carne","Porco","Frango")
start.values<-c(a1=0, a2=0, a3=0, a4=0, b1=1, b2=1, b3=1, b4=1,
               c11=0, c12=0, c13=0, c14=0, c21=0, c22=0, c23=0, c24=0,
               c31=0, c32=0, c33=0, c34=0, c41=0, c42=0, c43=0, c44=0)

model<-list(peixe.formula, carne.formula, porco.formula, frango.formula)

data2<-data[complete.cases(data),]

model.ols<-nlsystemfit(method="OLS", model, start.values, data=data2, eqnlabels=labels.list)
print(model.ols$b)
print(model.ols$p)

model02<-list(peixe.formula, carne.formula, porco.formula)
start.values02<-c(a1=0, a2=0, a3=0, b1=1, b2=1, b3=1, 
                c11=0, c12=0, c13=0, c14=0, c21=0, c22=0, c23=0, c24=0,
                c31=0, c32=0, c33=0, c34=0)
labels.list02<-list("Peixe","Carne","Porco")

model.sur<-nlsystemfit(method="SUR", model02, start.values02, data=data2, eqnlabels=labels.list02)
print(model.sur$b)
print(model.sur$p)

##################################
# AIDS
##################################

price.vars<-c("P_PEIXE", "P_CARNE", "P_PORCO", "P_FRANGO")
share.vars<-c("S_PEIXE", "S_CARNE", "S_PORCO", "S_FRANGO")

la.aids.result<-aidsEst(price.vars, share.vars, "valtot", data=data2, priceIndex="S")
print(la.aids.result)
summary(la.aids.result)

aids.consist<-aidsConsist(price.vars,"valtot", coef(la.aids.result), priceIndex = "S", data=data2)
print(aids.consist)

pMeans <- colMeans( data2[ , price.vars ] )
wMeans <- colMeans( data2[ , share.vars ] )

aidsElas(coef(la.aids.result), prices=pMeans, shares=wMeans)