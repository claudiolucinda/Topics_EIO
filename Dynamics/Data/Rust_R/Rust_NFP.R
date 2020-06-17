#########################################################
# Code for replicating Rust's (1987) analysis
# Using the NFP algorithm
# Claudio Lucinda, using code adapted by Wayne Taylor
# From original MATLAB codes of Victor Aguirregabiria
#########################################################

library(ggplot2)
library(msm)

setwd("G:/Meu Drive/Aulas/GV/Curso de OI - Pós/Mini Curso USP/Topics_EIO/Dynamics/Data/Single-Agent-Dynamic-Choice-master/Single-Agent-Dynamic-Choice-master")

rm(list=ls())

source("support_NFP.R")

#******************************
# 1. Reading Rust's bus data
#******************************
#Datafile
filedat  = "bus_df_lin.csv"
data = read.csv(filedat)

S = 70
# The parameters are arbitrarily chosen to ensure that the state transitions probabilities are plausible:

lower=0
upper=15000
mu=6000
sigma=4000

p_x0 = ptnorm(5000,mu,sigma,lower,upper)
p_x1 = ptnorm(10000,mu,sigma,lower,upper)-p_x0
p_x2 = 1 - p_x1 - p_x0
p = c(p_x0, p_x1, p_x2)
lin_cost=function(s,params) s*params[1]


############################################
# 2. Specification of relevant functions
# - Contraction Mapping
# - Log Likeliood function
############################################


contraction_mapping=function(S, p, MF, params, beta=0.75, threshold=1e-6, suppr_output=FALSE){
  
  "Compute the non-myopic expected value of the agent for each possible decision and each possible 
  state of the bus.
  Iterate until the difference in the previously obtained expected value and the new expected value
  is smaller than the threshold.

  Takes:
  * A finite number of states S
  * A state-transition probability vector p = [p(0), p(1), p(2), ..., p(k)] of length k < S
  * A maintenance cost function MF
  * A vector params for the cost function
  * A discount factor beta (optional)
  * A convergence threshold (optional)

  Returns:
  * The converged choice probabilities for the forward-looking and myopic agents for each state, 
  conditional on 'params'"
  
  achieved = TRUE
  
  # Initialization of the state-transition matrices
  # ST_mat: describe the state-transition probabilities if the maintenance cost is incurred
  # RT_mat: regenerate the state to 0 if the replacement cost is incurred.
  # [a,b] = transition from state "a" to "b"
  ST_mat = matrix(0,S,S)
  lp = length(p)
  for(i in 1:S){
    for(j in 1:lp){
      if((i+j-1)<S)  ST_mat[i,i+j-1] = p[j]
      if((i+j-1)==S) ST_mat[i,i+j-1] = sum(p[j:lp]) #out of columns, so collapse the probabilities
    }
  }
  
  R_mat = cbind(1,matrix(0,S,S-1))
  
  # Initialization of the expected value (which is also the myopic decision cost of the agent).
  # Here, the forward-looking component is initialized at 0.
  k = 0
  EV = matrix(0,S,2)
  EV_myopic = EV_new = myopic_costs(S, MF, params, p)
  
  # Contraction mapping loop
  while(max(abs(EV_new-EV)) > threshold){
    # Store the former expected value
    EV = EV_new
    # Obtained the probability of maintenance and replacement from the former expected value
    pchoice = choice_prob(EV)
    # Compute the expected cost for each state: Nx1 vector
    ecost = rowSums(pchoice*EV)
    # Compute the two components of forward-looking utility: In case of maintenance, 
    # utility of future states weighted by transition probabilities. In case of replacement,
    # the future utility is the utility of state 0
    futil_maint = ST_mat%*%ecost
    futil_repl = R_mat%*%ecost
    futil = cbind(futil_maint,futil_repl)
    # Future utility is discounted by beta, and added to the myopic cost. 
    EV_new = EV_myopic + beta*futil
    k = k+1
    if(k == 1000) achieved = FALSE
  }
  
  if(!suppr_output){
    if(achieved){
      cat("Convergence achieved in ",k," iterations")
    } else {
      cat("CM could not converge! Mean difference = ",round(mean(EV_new-EV),2))
    }
  }
  
  list(CP_forward=choice_prob(EV_new),CP_myopic=choice_prob(EV_myopic))
}

DynamicLogit=function(params,data,S,p,MF){
  
  "
  Evaluate the cost parameters underlying a bus replacement pattern by a forward-looking agent.
  
  Takes:
  * Data: a dataframe, which contains:
    -choice: the name of the column containing the dummy endogenous variable
    -state: the name of the column containing the exogenous variable 
  
  * p: The state-transition vector of exogenous variable.
      For instance, p = [0, 0.6, 0.4] means that the bus will 
      transition to the next mileage state with probability 0.6, 
      and to the second next mileage state with probability 0.4.
  
  * MF: A function passed as an argument, which is the functional 
      form for the maintenance cost. This function must accept as
      a first argument a state s, and as a second argument a vector of parameters.
  "
  
  endog = data$choice
  exog = data$state
  
  N=length(endog)
  S=max(exog)*2 # Assumes that the true maximum number states is twice the maximum observed state.
  
  # Matrices to speed up computations of the log-likelihood
  
  # A (SxN) matrix indicating the state of each observation
  state_mat=matrix(0,S,N)
  for(s in 0:(S-1)) state_mat[s+1,]=(exog==s)*1 #Note 0 is a state, sum(state_mat)==N should be true
  
  # A (2xN) matrix indicating with a dummy the decision taken by the agent for each time/bus observation (replace or maintain)
  dec_mat = rbind(t(1-endog),endog)
  
  "
  The log-likelihood of the Dynamic model is estimated in several steps.
  1) The current parameters are supplied to the contraction mapping function
  2) The function returns a matrix of decision probabilities for each state.
  3) This matrix is used to compute the loglikelihood of the observations
  4) The log-likelihood are then summed accross individuals, and returned
  "
  
  util = contraction_mapping(S=S, p=p, MF=MF, params=params, beta = .75,suppr_output = TRUE)
  pchoice = util$CP_forward
  logprob = log(t(state_mat)%*%pchoice)
  -sum(logprob*t(dec_mat))
}

# 2) Fitting the linear cost data

# A. Fitting the true linear cost function to the data

# In this section, we fit the data generated by the linear cost function and recover the parameters RC and ??. We do it for different characterizations of the cost function, and thereby illustrate the consequences of a misspecification.
# Recall that in the linear model:
# RC=20
# theta11=0.5

bounds = c(1e-6, Inf)
npars=2
lin_fit = optim(par=rep(.1,npars),fn=DynamicLogit,method=c("L-BFGS-B"),lower=bounds[1],upper=bounds[2],
                data=data,S=S,p=p,MF=lin_cost,control=list(fnscale=1, trace=1))

# Return the parameters obtained after fitting the likelihood function to the data.
loglike =  lin_fit$value
fit_params = lin_fit$par
cat("Log-Likelihood: ",loglike,fill=TRUE)
cat("RC: ",fit_params[1],fill=TRUE)
cat("thetas: ",fit_params[-1],fill=TRUE)

#Compare with the true values
params_lin = c(20,.5)
linEst = contraction_mapping(S=70, p=p, MF=lin_cost, params=fit_params, beta = .75)
lin_forwardEst=linEst$CP_forward
lin_myopicEst=linEst$CP_myopic

gglinEst = data.frame(decisionRule=c(rep("Forward-Looking (Lin Est.)",nrow(lin_forwardEst)),
                                     rep("Myopic (Lin Est.)",nrow(lin_myopicEst))),
                      pMaint=c(lin_forwardEst[,1],lin_myopicEst[,1]),
                      State=c(0:(nrow(lin_forwardEst)-1),0:(nrow(lin_myopicEst)-1)))

ggplot(gglinEst,aes(y=pMaint,x=State,color=decisionRule))+geom_line(lwd=1)+theme_bw(20)+xlim(5,50)