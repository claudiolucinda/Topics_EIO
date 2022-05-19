########################################
# Código para rodar o modelo de demanda
# Escolha Discreta com Dados Agregados
# Claudio R. Lucinda
# USP
# 2020
########################################
# SUGEST?O: Atualizar pra 3.6.0 o R
########################################

#install.packages("reticulate")
#install.packages("tidyverse")
#install.packages("SASxport")
#install.packages("Hmisc")
#install.packages("AER")
#install.packages("ivreg")
#install.packages("gmm")
#install.packages("plot.matrix")


library("reticulate")
library("tidyverse")
library("Hmisc")
library("SASxport")
library("AER")
library("gmm")
library("plot.matrix")

py_install("pyblp", pip = TRUE)
pyblp<-import("pyblp")

setwd("G:/Meu Drive/Aulas/GV/Curso de OI - Pós/Mini Curso USP/Topics_EIO/Demanda/Data")

dat<-sasxport.get("NevoData_OI.xpt")

dat<-dat %>%
  rename(price=v2, share=v1, sugar=v29, mushy=v30, cdid=v75, id=v76) 

data<-dat[,c("price", "share", "sugar", "mushy", "cdid", "id")]

data<-data %>%
  group_by(cdid) %>%
  mutate(inshare=sum(share))

data$outshare<-1-data$inshare

data$meanu<-log(data$share)-log(data$outshare)


# Basicão - OLS
model_0<-lm(meanu~price+sugar+mushy, data=data)
summary(model_0)

# Basicão - OLS + Efeitos Fixos de região
model_1<-lm(meanu~price+sugar+mushy+factor(cdid), data=data)
summary(model_1)

# Criando as dummies de produto
dum_data<-dat[,4:27]
teste <- simplify2array(
  apply(
    dum_data, 1, 
    function(x) paste(names(dum_data)[x != 0], collapse = " ")
  )
)
testevar<-data.frame(teste)
testevar$teste<-as.factor(testevar$teste)

data02<-data
data02$brand_id<-testevar$teste

# Dummies de Produto
model_2<-lm(meanu~price+sugar+mushy+factor(cdid)+factor(brand_id), data=data02)
summary(model_2)


# IVReg
instruments<-data.frame(dat[,32:75])
data03<-data.frame(data02,instruments)
inst_names<-colnames(instruments)
fla<-paste0("~",paste(inst_names, collapse="+"),"+sugar+mushy")

model_3<-ivreg(meanu~price+sugar+mushy,instruments=as.formula(fla), data=data03)
summary(model_3, diagnostics = TRUE)

####################################
# using PyBLP
####################################
pyblp$options$digits = 2L
pyblp.options.verbose = TRUE

# Renomeando stuff - Esses são nomes protegidos do 
# pacote

data.py<-data02 %>%
  rename(prices=price, market_ids=cdid,shares=share, 
         product_ids=brand_id)
# Variável 'prices' é assumida endógena. 
# Fiz uma volta pra replicar o model_0
data.py$demand_instruments0<-data.py$prices

# Versão pra replicar o model_0
logit_form01<-pyblp$Formulation('prices+sugar+mushy')
logit_prob01<-pyblp$Problem(logit_form01, data.py)
logit_solve01<-logit_prob01$solve()
logit_solve01
summary(model_0)

# Replicando o model_1
logit_form02<-pyblp$Formulation('prices+sugar+mushy', absorb = 'C(market_ids)')
logit_prob02<-pyblp$Problem(logit_form02, data.py)
logit_solve02<-logit_prob02$solve()
logit_solve02
# Comparação
summary(model_1)

# Replicando com IV
data.py2<-data03 %>%
  rename(prices=price, market_ids=cdid,shares=share, 
         product_ids=brand_id)

# essa volta porque tem que ser nome protegido pros instrumentos
# demand_instruments0, demand_instruments1...
names(data.py2)[11:ncol(data.py2)]<-paste0("demand_instruments",11:ncol(data.py2)-11)
data.py2<-data.py2[,1:30]

# tem que dar igual esse e o de baixo
logit_form03<-pyblp$Formulation('prices+sugar+mushy')
logit_prob03<-pyblp$Problem(logit_form03, data.py2)
logit_solve03<-logit_prob03$solve()
logit_solve03


# IVReg
data03<-data03[,1:30]
inst_names<-colnames(instruments)[1:20]
fla<-paste0("~",paste(inst_names, collapse="+"),"+sugar+mushy")

model_3<-ivreg(meanu~price+sugar+mushy,instruments=as.formula(fla), data=data03)
summary(model_3, diagnostics = TRUE)


#################################################################
# Nested Logit - Estrutura 1
#################################################################

# Aqui a estrutura é plana. Todos os produtos estão em um mesmo ninho, com 
# o outside good sendo o outro bem 
# sempre ajuda criar um instrumento que é o número de produtos por nest
# Só vou colocar uma variável - price

# data.py2<-data.py2 %>%
#   group_by('market_ids','nesting_ids') %>%
#   mutate(demand_instruments20=n(share))

solve_nl <- function(.df) {
  .df<-.df %>%
    group_by('market_ids','nesting_ids') %>%
    mutate(demand_instruments20=n())
  nl_formulation<-pyblp$Formulation('0+prices')
  problem<-pyblp$Problem(nl_formulation,.df)
  return(problem$solve(rho=.7))
}

data.py3<-data.py2
data.py3$nesting_ids<-1
nl_results1<-solve_nl(data.py3)
nl_results1
# Cálculo do Efeito marginal dos preços
nl_results1$beta[1]/(1-nl_results1$rho)


# Tá na trave, com o nesting parameter batendo perto do limite (rho=1)

# Nesting - versão 2
# Nesting baseado no mushy vs non-mushy
# Aqui o outside good tá no mesmo grupo do non-mushy

data.py4<-data.py2
data.py4$nesting_ids<-data.py4$mushy
nl_results2<-solve_nl(data.py4)
nl_results2
# Cálculo do Efeito marginal dos preços
nl_results2$beta[1]/(1-nl_results2$rho)

# Elasticidades - Para a gente ver como fica
elasts2<-nl_results2$compute_elasticities()

# Uma matriz por mercado, empilhada
single_mkt<-data.py4$market_ids==1

# Plotando elasticidades
elast_mkt1<-elasts2[single_mkt,]
plot(elast_mkt1)

# Logit Basicão
elasts03<-logit_solve03$compute_elasticities()
elast_mkt1_03<-elasts03[single_mkt,]
plot(elast_mkt1_03)

# Dá pra notar que é bem mais baixo....

###########################################
# RC Logit - Basicão, só pra mostrar
# as diferenças que a forma de integração
# é importante
###########################################
# Aqui você tem que separar a especificação
# da parte linear (os betas)
# da parte não-linear (os sigmas)
###########################################
X1<-pyblp$Formulation('0+prices', absorb='C(product_ids)')
X2<-pyblp$Formulation('1+prices+sugar+mushy')
RC0<-c(X1,X2)
mc_integration <- pyblp$Integration('monte_carlo', size=100L)
mc_problem = pyblp$Problem(RC0, data.py2, integration=mc_integration)
bfgs = pyblp$Optimization('bfgs', list('gtol'=1e-10))


RC_results1 = mc_problem$solve(sigma = rbind(c(1,1,1,1),
                                         c(1,1,1,1),
                                         c(1,1,1,1),
                                         c(1,1,1,1)), optimization=bfgs)
RC_results1

pr_integration <- pyblp$Integration('product', size=5L)
pr_problem = pyblp$Problem(RC0, data.py2, integration=pr_integration)
bfgs = pyblp$Optimization('bfgs', list('gtol'=1e-10))


RC_results2 = pr_problem$solve(sigma = rbind(c(1,1,1,1),
                                             c(1,1,1,1),
                                             c(1,1,1,1),
                                             c(1,1,1,1)), optimization=bfgs)
RC_results2

RC_results3 = pr_problem$solve(sigma = rbind(c(1,0,0,0),
                                             c(0,1,0,0),
                                             c(0,0,1,0),
                                             c(0,0,0,1)), optimization=bfgs)
RC_results3

# Não dá significante mesmo. Só para mostrar.

# Logit Basicão
elasts_RC<-RC_results1$compute_elasticities()
elast_mkt1_RC<-elasts_RC[single_mkt,]
plot(elast_mkt1_RC)






