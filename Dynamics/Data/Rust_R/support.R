##########################################################
# Supporting Functions for Rust's code replication
# Functions:
# clogit
# discthre
# kpie
# kernel
##########################################################

clogit=function(ydum,x,restx,namesb,sup_out=FALSE){
  
  "Maximum Likelihood estimation of McFadden's Conditional Logit
  Some parameters can be restricted
  Optimization algorithm: Newton's method with analytical 
  gradient and hessian
  
  Original code by Victor Aguirregabiria
  Converted into R by Wayne Taylor
  
  Last version:  12/7/2015


  Input       ydum    - (nobs x 1) vector of observations of dependet variable
                        Categorical variable with values: {1, 2, ..., nalt}

              x       - (nobs x (k * nalt)) matrix of explanatory variables
                        associated with unrestricted parameters.
                        First k columns correspond to alternative 1, and so on

              restx   - (nobs x nalt) vector of the sum of the explanatory
                        variables whose parameters are restricted to be
                        equal to 1.

              namesb  - (k x 1) vector with names of parameters

              sup_out - TRUE/FALSE indicates whether all output should be suppressed


  Output      best    - (k x 1) vector with ML estimates.

              varest  - (k x k) matrix with estimate of covariance matrix"
  
  cconvb = 1e-6
  myzero = 1e-16
  nobs = length(ydum)
  nalt = max(ydum)
  npar = ncol(x)/nalt
  if(npar!=length(namesb)) cat("ERROR: Dimensions of x and of names(b0) do not match ")
  
  xysum = rep(0,npar)
  j=1
  while(j<=nalt){
    xysum = xysum + colSums((ydum==j)*x[,(npar*(j-1)+1):(npar*j)])
    j=j+1
  }
  
  iter=1
  criter = 1000
  llike = -nobs
  b0 = matrix(0,npar,1)
  
  while(criter>cconvb){
    
    if(!sup_out){
      cat("Iteration                = ",iter,fill=TRUE)
      cat("Log-Likelihood function  = ",llike,fill=TRUE)
      cat("Norm of b(k)-b(k-1)      = ",criter,fill=TRUE)
    }
    
    #Computing probabilities
    phat = matrix(0,nobs,nalt)
    j=1
    while(j<=nalt){
      phat[,j] = x[,(npar*(j-1)+1):(npar*j)]%*%b0 + restx[,j]
      j=j+1
    }
    
    phat = phat - apply(phat,1,max)
    phat = exp(phat)/rowSums(exp(phat))
    
    #Computing xmean
    sumpx = matrix(0,nobs,npar)
    xxm = matrix(0,npar,npar)
    llike = 0
    j=1
    while(j<=nalt){
      xbuff = x[,(npar*(j-1)+1):(npar*j)]
      sumpx = sumpx + phat[,j]*xbuff
      xxm = xxm + t(phat[,j]*xbuff)%*%xbuff
      llike = llike+sum((ydum==j)*
                          log((phat[,j]>myzero)*phat[,j]
                              +(phat[,j]<myzero)*myzero))
      j=j+1
    }
    
    #Computing gradient
    d1llike = xysum - colSums(sumpx)
    
    #Computing hessian
    d2llike = - (xxm - t(sumpx)%*%sumpx)
    
    #Gauss iteration
    b1 = b0 - solve(d2llike)%*%d1llike
    criter = sqrt(t(b1-b0)%*%(b1-b0))
    b0 = b1
    iter = iter + 1
  }
  
  Avarb  = solve(-d2llike)
  sdb    = sqrt(diag(Avarb))
  tstat  = b0/sdb
  
  if(!sup_out){
    numyj  = as.vector(table(ydum))
    logL0  = sum(numyj*log(numyj/nobs))
    lrindex = 1 - llike/logL0
    
    cat("---------------------------------------------------------------------",fill=TRUE)
    cat("Number of Iterations     = ",iter,fill=TRUE)
    cat("Number of observations   = ",nobs,fill=TRUE)
    cat("Log-Likelihood function  = ",llike,fill=TRUE)
    cat("Likelihood Ratio Index   = ",lrindex,fill=TRUE)
    cat("---------------------------------------------------------------------",fill=TRUE)
    print(data.frame(Parameter=namesb,
                     Estimate=round(as.numeric(b0),3),
                     StandardErrors=round(sdb,3),
                     tratios=round(as.numeric(tstat),3)))
    cat("---------------------------------------------------------------------",fill=TRUE)
  }
  
  list(b0=b0,Avarb=Avarb)
}

discthre=function(y,thre){
  
  # DISCTHRE - Discretization and Codification of a Variable using a prefixed vector of thresholds.
  
  # Original code in GAUSS by Victor Aguirregabiria
  # Converted to R by Wayne Taylor
  
  # Version: 12/8/2015
  
  # Input       y    - (nobs x 1) vector with observations of the continuous variable
  #             thre - (k x 1) vector of thresholds
  
  # Output      discy - (nobs x 1) vector with the codes of the discretized observations
  #                     Example: If y[i]>thre[5] and y[i]<=thre[6], then discy[i]=6
  
  numcel = length(thre)
  discy = rep(0,length(y))
  discy = discy + 1*(y <= thre[1])
  
  j=2
  while(j <= numcel){
    discy = discy + j*(y>thre[j-1])*(y<=thre[j])
    j=j+1
  }
  
  discy = discy + (numcel+1)*(y>thre[numcel])
  
  discy  
}

kernel1=function(xobs,xpred){
  
  # KERNEL1 - Kernel Estimation of a Univariate Density Function using a Gaussian Kernel 
  # The bandwith is equal to Silverman's rule of thumb
  
  # Original code in GAUSS by Victor Aguirregabiria
  # Converted to R by Wayne Taylor
  
  # Version: 12/8/2015
  
  # Input:       xobs    - (N x 1) vector of observations
  #              xpred   - (K x 1) vector of values where the pdf will be estimated
  
  # Output:      pest    - (K x 1) vector of estimates
  
  nobs  = length(xobs)
  k = length(xpred)
  
  #Silverman's rule of thumb bandwidth
  band0 = (1.364 * sd(xobs))/((pi*nobs)^(1/5))
  
  pest = colSums(dnorm((matrix(xpred,nobs,k,byrow=TRUE)-xobs)/band0))/(nobs*band0)
  
  pest
}
