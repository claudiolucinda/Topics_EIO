function pest = kernel1(xobs,xpred)


%/*
%** KERNEL1 - Kernel Estimation of a Univariate Density Function 
%**           using a Gaussian Kernel 
%** 
%**           The bandwith is equal to Silverman's rule of thumb
%**           divided by "(pi*nobs)^(1/9)". That is, it deliberately
%**           produces under-smoothing
%**
%**
%** by Victor Aguirregabiria
%**
%** Format:      pest = kernel1(xobs,xpred)
%**
%** Input:       xobs    - (N x 1) vector of observations
%**              xpred   - (K x 1) vector of values where the pdf 
%**                        will be estimated
%**
%** Output:      pest    - (K x 1) vector of estimates
%**
%*/ Translation into MATLAB - Claudio R. Lucinda - University of Sao Paulo
  nobs  = size(xobs,1) ;
 %@ Silverman's rule of thumb bandwidth @
  band0 = (1.364 * std(xobs))/((pi*nobs)^(1/5)) ;  
  %@ Modified bandwidth @
  band0 = band0 /((pi*nobs)^(1/9)) ;    
  pest = sum(normpdf(((repmat(xpred',size(xobs,1),1) - repmat(xobs,1,size(xpred',2))))./band0))/(nobs*band0) ;
  