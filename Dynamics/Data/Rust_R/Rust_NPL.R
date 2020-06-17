####################################################################
# R Code for Estimating Rust (1987) Model using NPL Aguirregabiria and Mira (2002)
# Claudio Lucinda
# based on Wayne Taylor port of Victor Aguirregabiria MATLAB code
# 2020
#####################################################################

library(pracma)

setwd("G:/Meu Drive/Aulas/GV/Curso de OI - Pós/Mini Curso USP/Topics_EIO/Dynamics/Data/Single-Agent-Dynamic-Choice-master/Single-Agent-Dynamic-Choice-master")

rm(list=ls())

###############################
# Puxando funções
###############################

source("support.R")
#*********************
# 0. Some constants
#*********************

#Names of structural parameters
namespar = c("ReplaceC","MaintenC")

#Value of discount factor
beta = 0.75

#Number of state variables
kvarx = 1

#Number of choice alternatives
jchoice = 2

#Number of cells in discretization of the state variable
ncelx = 70

#The output from the accompanying DGP file is already discretized
#and the transition matrices are known so some steps below are not
#required. However, to understand the original function set the
#variable to TRUE to discretize x and estimate the transition matrices
orig_est = TRUE


#******************************
# 1. Reading Rust's bus data
#******************************
#Datafile
filedat  = "bus_df_lin.csv"
data = read.csv(filedat)

if(!orig_est){
  aobs = data$choice #Replacement decision  
  } else {
    data = data[order(data$id),]
    nobs = nrow(data)  #Cumulative mileage
    aobs = data$choice #Replacement decision  
    iobs = data$id     #Individual bus ID
    xobs = (data$state+runif(nobs))*1e6 #un-discretizing the data (to illustrate how the original code works)
    xobs = xobs/1e6
}

#********************************************************
# 2. Discretization of the decision and state variable
#********************************************************
indobsa = aobs+1 #indaobs should be 1,2,...,J

if(!orig_est){
  xval = 1:ncelx         #cost of maintaining at each state
  indobsx = data$state+1 #index starts at 1
} else{
  minx = quantile(xobs,0.01)
  maxx = quantile(xobs,0.99)
  stepx = round(1e6*(maxx-minx)/(ncelx-1))/1e6
  xthre = seq(minx+stepx,by=stepx,length.out=ncelx-1)
  xval = seq(minx,by=stepx,length.out=ncelx)
  indobsx = discthre(xobs,xthre)
  head(indobsx)
}

#****************************************
# 3. Specification of utility function
#****************************************
zmat1= cbind(0,-xval)            #if maintain (zmat1), replace cost = 0 and maintain cost = -xval
zmat2= cbind(-1,rep(0,ncelx))    #if replace (zmat2), replace cost = 1 and maintain cost = 0
zmat=cbind(zmat1,zmat2)
rm(zmat1,zmat2)

#****************************************************************
# 4. Estimation of transition probabilities of state variables
#****************************************************************

if(!orig_est){
  fmat1 = matrix(0,ncelx,ncelx)
  p=c(.36,.48,.16)
  lp = length(p)
  for(i in 1:ncelx){
    for(j in 1:lp){
      if((i+j-1)<ncelx)  fmat1[i,i+j-1] = p[j]
      if((i+j-1)==ncelx) fmat1[i,i+j-1] = sum(p[j:lp]) #out of columns, so collapse the probabilities
    }
  }
  fmat2 = cbind(1,matrix(0,ncelx,ncelx-1))
  fmat=cbind(fmat1,fmat2)
} else {
  # Nonparametric PDF of additional mileage
  iobs_1 = c(0,iobs[-nobs])
  xobs_1 = c(0,xobs[-nobs])
  aobs_1 = c(0,aobs[-nobs])
  dxobs = (1-aobs_1)*(xobs-xobs_1) + aobs_1*xobs
  dxobs = dxobs[iobs==iobs_1]
  mindx = 0
  maxdx = quantile(dxobs,0.999)
  numdx = 2 + round(maxdx/stepx)
  dxval = seq(0,by=stepx,length.out=numdx)
  pdfdx = kernel1(dxobs,dxval)
  pdfdx = pdfdx/sum(pdfdx)
  
  # Transition matrices
  fmat2 = kronecker(matrix(1,ncelx,1),t(c(pdfdx,rep(0,ncelx-numdx))))
  
  fmat1 = t(c(pdfdx,rep(0,ncelx-numdx)))
  j=2
  while(j<=(ncelx-1)){
    colz = ncelx - (j-1+numdx)
    if (colz>0) fmat1 = rbind(fmat1,c(rep(0,j-1),pdfdx,rep(0,colz)))
    if (colz==0) fmat1 = rbind(fmat1,c(rep(0,j-1),pdfdx))
    if (colz<0){
      buff = c(pdfdx[1:(numdx+colz-1)],sum(pdfdx[(numdx+colz):numdx]))
      fmat1 = rbind(fmat1,c(rep(0,j-1),buff))
    }
    j=j+1
  }
  fmat1 = rbind(fmat1,c(rep(0,ncelx-1),1))
  
  fmat=cbind(fmat1,fmat2)
}

#***********************************
# 5. Initial choice probabilities
#***********************************
xprob0=cbind(indobsx,indobsx^2,indobsx^3)
glmout=glm(aobs~xprob0-1,family='binomial')
coef=glmout$coef
est = exp(-(cbind(xval,xval^2,xval^3)%*%coef))
prob0 = 1/(1+est)                              #pr(replace) (columnas)|state (rows)
prob0 = cbind(1-prob0,prob0)                   #probability of maintain[,1] or replace[,2]|state 

#############################################
# NPL Estimation
#############################################
npl_sing=function(inda,indx,zmat,pini,bdisc,fmat,names){
  
  "
  Maximum Likelihood Estimates of structural parameters 
  of a discrete choice single-agent dynamic programming 
  model using the NPL algorithm in Aguirregabiria and Mira (Econometrica, 2002)
   
  Original code in GAUSS by Victor Aguirregabiria
  Converted to R by Wayne Taylor

  Version 12/7/2015
  ---------------------------------------------------------------
  
  INPUTS:
    inda    - (nobs x 1) vector with indexes of discrete decision variable (values of 1,...,J)
  
    indx    - (nobs x 1) vector with indexes of the state vector x (values of 1,..,S)
  
    zmat    - (zmat1,zmat2,...,zmatJ) matrix with the values of the variables z(a=j,x)
                note: each zmat has J columns to represent the utility of choice j given action a
  
    pini    - (numx x J) vector with the initial estimates of the choice probabilities Pr(a=j|x)
  
    bdisc   - Discount factor (between 0 and 1)
  
    fmat    - (fmat1,fmat2,...,fmatJ) matrix with the conditional choice transition probs
  
    names   - (npar x 1) vector with names of parameters
  
   OUTPUTS:
    A list of size K where the k'th entry contains:
    
    tetaest - (npar x 1) matrix with estimates of structural parameters of the k'th stage estimate
  
    varest  - (npar x npar) matrix with asymptotic covariance matrices of estimates for the k'th stage
  
    pest    - (numx x J) matrix with the estimated choice probabilities Pr(d=1|x),...,Pr(d=J|x) for the k'th stage
  ---------------------------------------------------------------"
  
  npar = length(names)
  nobs = length(inda)
  nchoice = max(inda)
  if(ncol(zmat)!=(npar*nchoice)){
    print("Error: The number of columns in 'zmat' does not agree",fill=TRUE)
    print("with the number of 'choices * number of parameters'",fill=TRUE)
  }
  
  myzero = 1e-12
  eulerc = 0.5772
  numx = nrow(pini)
  convcrit = 1000
  convcons = 1e-6
  tetaest0 = matrix(0,npar,1)
  out = NULL
  
  #---------------------------------------------------------
  #             ESTIMATION OF STRUCTURAL PARAMETERS
  #---------------------------------------------------------
  ks=1
  while(convcrit>=convcons){
    
    cat("-----------------------------------------------------",fill=TRUE)
    cat("POLICY ITERATION ESTIMATOR: STAGE =",ks,fill=TRUE)
    cat("-----------------------------------------------------",fill=TRUE)
    
    #1. Obtaining matrices "A=(I-beta*Fu)" and "Bz=sumj{Pj*Zj}" and vector Be=sumj{Pj*ej}
    #-----------------------------------------------------------------------------------
    
    i_fu = matrix(0,numx,numx)
    sumpz = matrix(0,numx,npar)
    sumpe = matrix(0,numx,1)
    j=1
    while (j<=nchoice){
      i_fu = i_fu + pini[,j]*fmat[,(numx*(j-1)+1):(numx*j)] #notice the column references
      sumpz = sumpz + pini[,j]*zmat[,(npar*(j-1)+1):(npar*j)]
      sumpe = sumpe + pini[,j]*(eulerc - log(pini[,j]+myzero)) #NOTE  I ADDED +MYZERO so log() works
      j=j+1 ;
    }
    
    i_fu = diag(numx) - bdisc * i_fu
    
    #2. Solving the linear systems "A*Wz = Bz" and "A*We = Be" using CROUT decomposition
    #-----------------------------------------------------------------------------------
    
    i_fu = lu(i_fu)
    wz = solve(i_fu$L,cbind(sumpz,sumpe))
    wz = solve(i_fu$U,wz)
    
    we = wz[,npar+1]
    wz = wz[,1:npar]
    
    #OR:
    # we=solve(i_fu,sumpe)
    # wz=solve(i_fu,sumpz)
    
    #3. Computing "ztilda(a,x) = z(a,x) + beta * F(a,x)'*Wz" and "etilda(a,x) = beta * F(a,x)'*We"
    #-----------------------------------------------------------------------------------
    
    ztilda = matrix(0,numx,nchoice*npar)
    etilda = matrix(0,numx,nchoice)
    j=1
    while(j<=nchoice){
      ztilda[,(npar*(j-1)+1):(npar*j)] = zmat[,(npar*(j-1)+1):(npar*j)]+bdisc*fmat[,(numx*(j-1)+1):(numx*j)]%*%wz
      etilda[,j] = bdisc * fmat[,(numx*(j-1)+1):(numx*j)]%*%we  
      j=j+1
    }
    
    #4. Sample observations of "ztilda" and "etilda"
    #-----------------------------------------------------------------------------------
    
    zobs = ztilda[indx,]
    eobs = etilda[indx,]
    
    #-----------------------------------------------------------------------------------
    #5. Pseudo Maximum Likelihood Estimation
    
    clogitout=clogit(inda,zobs,eobs,names)
    tetaest1=clogitout$b0
    varest=clogitout$Avarb
    
    #6. Re-Computing probabilities
    #-----------------------------------------------------------------------------------
    
    pini = matrix(0,numx,nchoice)
    j=1
    while(j<=nchoice){
      pini[,j] = ztilda[,(npar*(j-1)+1):(npar*j)]%*%tetaest1 + etilda[,j]
      j=j+1
    }
    pini = pini - apply(pini,1,max)
    pini = exp(pini)
    pini = pini/rowSums(pini)
    
    #7. Convergence Criterion
    #-----------------------------------------------------------------------------------
    convcrit = max(abs(tetaest1-tetaest0))
    tetaest0 = tetaest1
    cat("NPL Criterion =",convcrit,fill=TRUE)
    
    #8. Save output from current k'th stage
    #------------------------------------------------------------------------------------
    out[[ks]]=list(tetaest=tetaest1,varest=varest,pini=pini)
    
    ks=ks+1
  }
  
  out
}

#***************************
# 6. Structural estimation
# ***************************
out = npl_sing(indobsa,indobsx,zmat,prob0,beta,fmat,namespar)

out[[length(out)]][[1]]

# Bus data true parameters: MC = 20 RC = .5

# orig_est=FALSE
# [1,] 19.2014940
# [2,]  0.5233962

# orig_est=TRUE
# [1,] 17.8076640
# [2,]  0.5922635
