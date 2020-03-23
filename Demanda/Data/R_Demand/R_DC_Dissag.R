#####################################################
# Código R para os modelos de escolha discreta com 
# Dados desagregados
# Claudio R. Lucinda
# USP - 2020
#####################################################

#install.packages("mlogit")
library("mlogit")

# Importando os dados
data<-read.delim("..\\mgdata5.txt", header=FALSE, sep="\t")

# Estrutura do Banco
# 
# 1. Panelist
# 2. Category purchase? 0 - No; 1 - Yes
# 3. Week
# 4-7. Brand bought if any
# 8-11. Price
# 12-15. Feature
# 16-19. Display

names(data)[1]<-"Panelist"
names(data)[2]<-"Purchase"
names(data)[3]<-"Week"
names(data)[4]<-"Brand_1"
names(data)[5]<-"Brand_2"
names(data)[6]<-"Brand_3"
names(data)[7]<-"Brand_4"
names(data)[8]<-"Price_1"
names(data)[9]<-"Price_2"
names(data)[10]<-"Price_3"
names(data)[11]<-"Price_4"
names(data)[12]<-"Feature_1"
names(data)[13]<-"Feature_2"
names(data)[14]<-"Feature_3"
names(data)[15]<-"Feature_4"
names(data)[16]<-"Display_1"
names(data)[17]<-"Display_2"
names(data)[18]<-"Display_3"
names(data)[19]<-"Display_4"

# Removendo linhas com panelist e week duplicado
data2<-data[!(duplicated(data[c("Panelist", "Week")])),]

data2$choice<-data2$Brand_1+2*data2$Brand_2+3*data2$Brand_3+4*data2$Brand_4
data2$id<-seq.int(nrow(data2))

Dat<-mlogit.data(data=data2, shape="wide", choice="choice",
                 varying=8:19, sep="_", id.var="id")

f <-mlogit(choice ~ Price + Feature + Display, data=Dat)
summary(f)

# Random Coefficient Models

RC_f<-mlogit(choice ~ Price + Feature + Display, data=Dat,
             panel = FALSE, rpar = c(Price = "n"), R = 100,
             method = "bhhh")
summary(RC_f)

# Aqui vemos que existe a possibilidade - entre a média e o 3º quartil
# de você ter uma demanda que sobe.
mg_Price<-rpar(RC_f, "Price")

# Random Coefficient Models - Lognormal
# O opposite aqui é necessário porque o modelo espera que Price tenha um coeficiente 
# negativo e um RC lognormal

Dat2<-mlogit.data(data=data2, shape="wide", choice="choice",
                 varying=8:19, sep="_", id.var="id", opposite = c("Price"))

RC_f2<-mlogit(choice ~ Price + Feature + Display, data=Dat2,
             panel = FALSE, rpar = c(Price = 'ln'), R = 100, halton=NA,
             method = "bhhh")
summary(RC_f2)

# Aqui vemos que existe a possibilidade - entre a média e o 3º quartil
# de você ter uma demanda que sobe.
mg_Price_2<-rpar(RC_f2, "Price")
summary(mg_Price_2)