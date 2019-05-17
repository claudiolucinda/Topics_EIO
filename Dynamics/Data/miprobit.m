function [b0,Avarb,llike] = miprobit(ydum,x,rest,b0,nombres,out)
% // -----------------------------------------------------------------------
% // MIPROBIT - Estimation of a Probit Model by Maximum Likelihood
% //            The optimization algorithm is a Newton's method
% //            with analytical gradient and hessian
% //
% // FORMAT    {best,varest,llike} = miprobit(ydum,x,rest,b0,nombres,out)
% //
% // INPUTS
% //      ydum    - (nobs x 1) vector with observations of the dependent variable
% //      x       - (nobs x k) matrix with observations of explanatory variables
% //                associated with the unrestricted parameters
% //      rest    - vector with observations of the sum of the explanatory
% //                variables whose parameters are restricted to be 1
% //                (Note that the value 1 is without loss of generality
% //                if the variable rest is constructed appropriately)
% //      b0      - (k x 1) vector with values of parameters to initialized 
% //                Newton's methos
% //      nombres - (k x 1) vector with names of parameters to estimate
% //      out     - 0=no table of results; 1=table with estimation results
% //
% //  OUTPUTS
% //      best    - ML estimates
% //      varest  - estimate of the covariance matrix
% //      llike   - value of log-likelihood function at the MLE
% // -----------------------------------------------------------------------

%   local myzero, nobs, nparam, eps, iter, llike,
%         criter, Fxb0, phixb0, lamdab0, dlogLb0,
%         d2logLb0, b1, lamda0, lamda1, Avarb, sdb, tstat,
%         numy1, numy0, logL0, LRI, pseudoR2, k ;
  myzero = 1e-36 ;
  nobs = size(ydum,1) ;
  nparam = size(x,2) ;
  eps = 1E-6 ;
  iter=1 ;
  llike = 1000 ;
  criter = 1000 ;
  while (criter>eps) ;
    if (out==1) ;
%       disp(['Pseudo MLE Iteration     = ' num2str(iter)]) ;
%       disp(['Log-Likelihood function  = ' num2str(llike)]) ;
%       disp(['Criterion                = ' num2str(criter)]);
     end ;
    Fxb0 = normcdf(x*b0+rest,0,1) ;
    Fxb0 = Fxb0 + (myzero - Fxb0).*(Fxb0<myzero)+(1-myzero - Fxb0).*(Fxb0>(1-myzero));
    llike = ydum'*log(Fxb0) + (1-ydum)'*log(1-Fxb0) ;
    phixb0 = normpdf(x*b0+rest,0,1) ;
    lamdab0 = ydum.*(phixb0./Fxb0) + (1-ydum).*(-phixb0./(1-Fxb0)) ;
    dlogLb0 = x'*lamdab0 ;
    d2logLb0 = -(repmat((lamdab0.*(lamdab0 + x*b0 + rest)),1,size(x,2)).*x)'*x ;
    %d2logLb0=-(repmat((lamdab0.*(lamdab0 + x*b0 + rest)),1,size(x,2)).*x)'*x ;
    b1 = b0 - inv(d2logLb0)*dlogLb0 ;
    criter = max(abs(b1-b0)) ;
    b0 = b1 ;
    iter = iter + 1 ;
  end ;
  Fxb0 = normcdf(x*b0 + rest,0,1) ;
  Fxb0 = Fxb0 + (myzero - Fxb0).*(Fxb0<myzero)+(1-myzero - Fxb0).*(Fxb0>(1-myzero));
  llike = ydum'*log(Fxb0) + (1-ydum)'*log(1-Fxb0) ;
  phixb0 = normpdf(x*b0 + rest,0,1) ;
  lamda0 = -phixb0./(1-Fxb0) ;
  lamda1 =  phixb0./Fxb0 ;
  Avarb  = (repmat((lamda0.*lamda1),1,size(x,2)).*x)'*x ;
  %Avarb  = ((lamda0.*lamda1).*x)'*x ;
  Avarb  = inv(-Avarb) ;
  sdb    = sqrt(diag(Avarb)) ;
  tstat  = b0./sdb ;
  numy1  = sum(ydum) ;
  numy0  = nobs - numy1 ;
  logL0  = numy1*log(numy1) + numy0*log(numy0) - nobs*log(nobs) ;
  LRI    = 1 - llike/logL0 ;
  pseudoR2 = 1 - ( (ydum - Fxb0)'*(ydum - Fxb0) )/numy1 ;
  if (out==1) ;
    disp(['Number of Iterations     = ' num2str(iter)]) ;
    disp(['Log-Likelihood function  = ' num2str(llike)]) ;
    disp(['Likelihood Ratio Index   = ' num2str(LRI)]) ;
    disp(['Pseudo-R2                = ' num2str(pseudoR2)]) ;
    disp('------------------------------------------------------------------');
    disp('       Parameter     Estimate        Standard        t-ratios');
    disp('                                     Errors') ;
    disp('------------------------------------------------------------------');
    k=1;
    while k<=nparam;
      disp([nombres(k) num2str(b0(k),'%10.2f') num2str(sdb(k)) num2str(tstat(k))]);
      k=k+1 ;
    end;
    disp('------------------------------------------------------------------');
    
  end ;
  

