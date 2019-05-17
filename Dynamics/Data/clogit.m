function [b0,Avarb]=clogit(ydum,x,restx,namesb)

% /*
% ** CLOGIT  -  Maximum Likelihood estimation of McFadden's Conditional Logit
% **            Some parameters can be restricted
% **            Optimization algorithm: Newton's method with analytical 
% **            gradient and hessian
% **
% ** by Victor Aguirregabiria
% **
% ** Format      {best,varest} = clogit(ydum,x,restx,namesb)
% **
% ** Input        ydum    - (nobs x 1) vector of observations of dependet variable
% **                        Categorical variable with values: {1, 2, ..., nalt}
% **
% **              x       - (nobs x (k * nalt)) matrix of explanatory variables
% **                        associated with unrestricted parameters.
% **                        First k columns correspond to alternative 1, and so on
% **
% **              restx   - (nobs x nalt) vector of the sum of the explanatory
% **                        variables whose parameters are restricted to be
% **                        equal to 1.
% **
% **              namesb  - (k x 1) vector with names of parameters
% **
% **
% **  Output      best    - (k x 1) vector with ML estimates.
% **
% **              varest  - (k x k) matrix with estimate of covariance matrix
% **
% */
% MATLAB Translation - Claudio R. Lucinda - University of Sao Paulo


  cconvb = 1e-6 ;
  myzero = 1e-16 ;
  nobs = size(ydum,1) ;
  nalt = max(ydum) ;
  npar = size(x,2)/nalt ;
  if npar~=size(namesb,1) ;
    disp(['ERROR: Dimensions of x' num1str(npar) 'and of names(b0)' num2str(size(namesb,1)) 'do not match ']) ;
  return;
  end;

  xysum = 0 ;
  j=1;
  while j<=nalt ;
    xysum = xysum + sum(repmat(ydum==j,1,npar).*x(:,npar*(j-1)+1:npar*j) )' ;
    j=j+1 ;
  end;

  iter=1 ;
  criter = 1000 ;
  llike = -nobs ;
  b0 = zeros(npar,1) ;
  %itmax=50;

  while (criter>cconvb); % && iter<itmax;
%     disp(['Iteration                = ' num2str(iter)]);
%     disp(['Log-Likelihood function  = ' num2str(llike)]);
%     disp(['Norm of b(k)-b(k-1)      = ' num2str(criter)]);
%     
    %@ Computing probabilities @
    phat = zeros(nobs,nalt) ;
    j=1 ;
    while j<=nalt ;
      phat(:,j) = x(:,npar*(j-1)+1:npar*j)*b0 + restx(:,j) ;
      j=j+1 ;
    end ;
    phat = phat - repmat(max(phat,[],2),1,size(phat,2)) ;
    phat = exp(phat)./repmat(sum(exp(phat),2),1,size(exp(phat),2)) ;

    %@ Computing xmean @
    sumpx = zeros(nobs,npar) ;
    xxm = 0 ;
    llike = 0 ;
    j=1;
    while j<=nalt ;
      xbuff = x(:,npar*(j-1)+1:npar*j) ; 
      sumpx = sumpx + repmat(phat(:,j),1,size(xbuff,2)).*xbuff ;
      xxm = xxm + (repmat(phat(:,j),1,size(xbuff,2)).*xbuff)'*xbuff ;
      llike = llike+sum((ydum==j).*log(+(phat(:,j)> myzero).*phat(:,j)+(phat(:,j)<=myzero).*myzero));
      j=j+1 ;
    end;

    %@ Computing gradient @
    d1llike = xysum - sum(sumpx,1)' ;

    %@ Computing hessian @
    d2llike = - (xxm - sumpx'*sumpx) ;

    %@ Gauss iteration @
    b1 = b0 - (d2llike)\d1llike ;
    criter = sqrt( (b1-b0)'*(b1-b0) ) ;
    b0 = b1 ;
    iter = iter + 1 ;
  end;

  Avarb  = inv(-d2llike) ;
  sdb    = sqrt(diag(Avarb)) ;
  tstat  = b0./sdb ;
  disp('---------------------------------------------------------------------');
  disp(['Number of Iterations     = ' num2str(iter)]);
  disp(['Log-Likelihood function  = ' num2str(llike)]);
  disp('---------------------------------------------------------------------');
  disp('       Parameter         Estimate        Standard        t-ratios');
  disp('                                         Errors') ;
  disp('---------------------------------------------------------------------');
  j=1;
  while j<=npar;
              disp([namesb(j) num2str(b0(j)) num2str(sdb(j)) num2str(tstat(j))]) ;
    j=j+1 ;
  end;
  disp('---------------------------------------------------------------------');
