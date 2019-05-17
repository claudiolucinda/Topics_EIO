function [prob1] = freqprob(yobs,xobs,xval)
% // -----------------------------------------------------------------------
% //  FREQPROB.SRC   Procedure that obtains a frequency estimation
% //                 of Prob(Y|X) where Y is a vector of binary 
% //                 variables and X is a vector of discrete variables
% //  FORMAT:
% //      freqp = freqprob(yobs,xobs,xval) 
% //
% //  INPUTS:
% //      yobs    - (nobs x q) vector with sample observations 
% //                of Y = Y1 ~ Y2 ~ ... ~ Yq
% //            
% //      xobs    - (nobs x k) matrix with sample observations of X
% //
% //      xval    - (numx x k) matrix with the values of X for which
% //                we want to estimate Prob(Y|X).
% needs   numx, numq, prob1, t, selx, denom, numer floating around as
% locals in GAUSS. they are wrapped up in a structure called argums
% //
% //  OUTPUTS:
% //      freqp   - (numx x q) vector with frequency estimates of
% //                Pr(Y|X) for each value in xval.
% //                Pr(Y1=1|X) ~ Pr(Y2=1|X) ~ ... ~ Pr(Yq=1|X) 
% // -----------------------------------------------------------------------
% Translation into MATLAB - Claudio Lucinda - University of Sao Paulo

  numx = size(xval,1) ;
  numq = size(yobs,2) ;
  prob1 = zeros(numx,numq) ;
  t=1 ;
  while t<=numx ;
    selx= prod(+(xobs==repmat(xval(t,:),size(xobs,1),1)),2);
    denom = sum(selx) ;
    if (denom==0) ;
      prob1(t,:) = zeros(1,numq) ;
    else
      numer = sum(repmat(selx,1,numq).*yobs) ;
      prob1(t,:) = (numer')./denom ;
    end
    t=t+1 ;
  end 
  
