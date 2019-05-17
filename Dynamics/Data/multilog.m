function [b0,Avarb]=multilog(yobs,xobs)

% Multinomial Logit Estimation
% GAUSS Version - Victor Aguirregabiria - University of Toronto
% MATLAB translation - Claudio Lucinda - University of Sao Paulo
  nobs = size(yobs,1) ;
  kparam = size(xobs,2) ;
  nalt = max(yobs) ;
  nparam = kparam*(nalt-1) ;
  eps1 = 1E-6 ;
  pt1=cellstr(strcat('b',num2str((1:1:nalt)')));
  pt2=cellstr(strcat('x',num2str((1:1:kparam)')));
  namesb={};
  for k=1:length(pt1)
      namesb=cat(2,namesb,strcat(pt1{k,1},pt2));
  end
         
  disp('     --------------------------------------------------------');
  disp('     REDUCED FORM ESTIMATION CONDITIONAL CHOICE PROBABILITIES');
  disp('     --------------------------------------------------------');
  iter=1 ;
  criter1 = 1000 ;
  criter2 = 1000 ;
  b0 = zeros(kparam*(nalt-1),1) ;

  
  while (criter1>eps1) ;
    Fxb0 = promlog(nalt,xobs,b0) ;
    [~, d1like, d2like]= logmlog(yobs,xobs,Fxb0) ;

%     disp(['Iteration                = ' num2str(iter)]);
%     disp(['Log-Likelihood function  = ' num2str(llike)]);
%     disp(['Norm of b(k)-b(k-1)      = ' num2str(criter1)]);
    b1 = b0 - (d2like)\d1like ;
    criter1 = sqrt( (b1-b0)'*(b1-b0) ) ;
    b0 = b1 ;
    iter = iter + 1 ;
  end;

  Fxb0 = promlog(nalt,xobs,b0) ;
  [llike, ~, d2like]= logmlog(yobs,xobs,Fxb0) ;

  Avarb  = inv(-d2like);
  sdb    = sqrt(diag(Avarb)) ;
  tstat  = b0./sdb ;
  b0buff  = [zeros(kparam,1) reshape(b0,nalt-1,kparam)'];
  sdbbuff = [zeros(kparam,1) reshape(sdb,nalt-1,kparam)'];
  tbuff   = [zeros(kparam,1) reshape(tstat,nalt-1,kparam)'];


  disp('--------------------------------------------------------------------');
  disp(['Number of Iterations     = ' num2str(iter)]);
  disp(['Log-Likelihood function  = ' num2str(llike)]);
  disp('--------------------------------------------------------------------');
  disp('         Parameter         Estimate        Standard        t-ratios');
  disp('                                            Errors') ;
  disp('--------------------------------------------------------------------');
  j=2;
  while j<=nalt ;
    k=1 ;
    while k<=kparam ;
        %resolver isso pra imprimir bonitinho....
        disp([namesb(k,j) num2str(b0buff(k,j)) num2str(sdbbuff(k,j)) num2str(tbuff(k,j))]) ;
      k=k+1 ;
    end ;
  disp('--------------------------------------------------------------------');    
    j=j+1 ;
  end;

  