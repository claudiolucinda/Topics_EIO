function [tetaest,varest, pest]=kpie(inda,indx,zmat,pini,bdisc,fmat,kstage,names)
% /*
% ** ---------------------------------------------------------------
% ** KPIE.SRC - Estimates structural parameters of a 
% **            discrete choice dynamic programming model
% **            using the K-stage Policy iterarion estimator (NPL)
% **            in Aguirregabiria and Mira (Econometrica, 2002)
% **
% ** by Victor Aguirregabiria
% **
% ** ---------------------------------------------------------------
% **
% **  Format of the procedure:
% **
% ** { tetaest , varest, pest } =
% **          kpie(inda,indx,zmat,pini,bdisc,fmat,kstage,names)
% **
% **  INPUTS:
% **          inda    - (nobs x 1) vector with indexes of discrete 
% **                    decision variable
% **
% **          indx    - (nobs x 1) vector with indexes of the state 
% **                    vector x
% **
% **          zmat    - (zmat1 ~ zmat2 ~ ... ~ zmatJ) matrix with the 
% **                    values of the functions z(a,x) 
% **
% **          pini    - (numx x J) vector with the initial reduced form
% **                    estimates of the choice probabilities Pr(a=j|x)
% **
% **          bdisc   - Discount factor (between 0 and 1)
% **
% **          fmat    - (fmat1 ~ fmat2 ~ ... ~ fmatJ) matrix with the 
% **                    conditional choice transition probs
% **
% **          kstage  - Number of "outer" policy iterations. If kstage>=5 
% **                    the procedure iterates until convergence and returns 
% **                    the maximum likelihood estimator.
% **
% **          names   - (npar x 1) vector with names of parameters
% **
% **  OUTPUTS:
% **          tetaest - (npar x kstage) matrix with estimates of structural 
% **                    parameters for each of the K stages
% **
% **          varest  - (npar x npar*kstage) matrix with asymptotic covariance 
% **                    matrices of estimates for each of the K stages
% **
% **          pest    - (numx x kstage) matrix with the estimated choice
% **                    probability Pr(d=1|x) associated with each of 
% **                    the kstage estimators
% ** ---------------------------------------------------------------
% MATLAB Translation - Claudio R. Lucinda - University of Sao Paulo

% proc (3) = kpie(inda,indx,zmat,pini,bdisc,fmat,kstage,names) ;
%   local npar, nobs, nchoice, myzero, eulerc, numx, 
%         criter, cconv,
%         tetaest, varest, pest, ks, sumpz, sumpe, i_fu, j, 
%         wz, we, ztilda, etilda, zobs, eobs,
%         teta0, var0 , flagc ;
          
  npar = size(names,1) ;
  nobs = size(inda,1) ;
  nchoice = max(inda) ;
  if size(zmat,2)~=(npar*nchoice) ;
    disp('Error: The number of columns in "zmat" does not agree') ;
    disp('with the number of "choices * number of parameters"') ;
    return;
  end;
    
  myzero = 1e-12 ;
  eulerc = 0.5772 ;
  numx = size(pini,1) ;
  tetaest = zeros(npar,kstage) ;
  varest = zeros(npar,npar*kstage) ;
  pest = zeros(numx,nchoice*kstage) ;
  
  disp('     ---------------------------------------------------------');
  disp('             ESTIMATION OF STRUCTURAL PARAMETERS');
  disp('     ---------------------------------------------------------');
  criter = 1000 ;
  cconv = 1e-6 ;
  ks=1 ;
  while (ks<=kstage) && (criter>cconv) ;
    disp(' -----------------------------------------------------');
    disp([' POLICY ITERATION ESTIMATOR: STAGE = ' num2str(ks)]) ;
    disp(' -----------------------------------------------------');
%     @ ---------------------------------------------------------- @
%     @ 1. Obtaining matrices "A=(I-beta*Fu)" and "Bz=sumj{Pj*Zj}" @
%     @    and vector Be=sumj{Pj*ej}                               @
%     @ ---------------------------------------------------------- @

    pini = (pini>=myzero).*(pini<=(1-myzero)).*pini+(pini<myzero).*myzero ...
        + (pini>(1-myzero)).*(1-myzero) ;

    i_fu = zeros(numx,numx) ;
    sumpz = zeros(numx,npar) ;
    sumpe = zeros(numx,1) ;  
    j=1 ;
    while j<=nchoice ;
      i_fu = i_fu + repmat(pini(:,j),1,numx).*fmat(:,numx*(j-1)+1:numx*j) ;
      sumpz = sumpz + repmat(pini(:,j),1,npar).*zmat(:,npar*(j-1)+1:npar*j) ;
      sumpe = sumpe + pini(:,j).*(eulerc - log(pini(:,j))) ;
      j=j+1 ;
    end ;
    i_fu = eye(numx) - bdisc * i_fu ;
    
%     @ ----------------------------------------------------------@
%     @ 2. Solving the linear systems "A*Wz = Bz" and "A*We = Be" @
%     @    using CROUT decomposition                              @
%     @ ----------------------------------------------------------@

% using the LU factorization instead

    %[i_fu_l,i_fu_u]=lu(i_fu) ;
    %opts.LT=true;
    %wz = linsolve(i_fu_l,[sumpz sumpe],opts) ;
    %wz=pinv(i_fu_l)*[sumpz sumpe];
    %opts2.UT=true;
    %wz = linsolve(i_fu_u,wz,opts2) ;
    wz=(i_fu)\[sumpz sumpe];
    
    clear i_fu sumpz sumpe
    
    we = wz(:,npar+1) ;
    wz = wz(:,1:npar) ;
      
%     @ --------------------------------------------------------@
%     @ 3. Computing "ztilda(a,x) = z(a,x) + beta * F(a,x)'*Wz" @
%     @    and "etilda(a,x) = beta * F(a,x)'*We"                @
%     @ ------------------------------------------------------- @
    ztilda = zeros(numx,nchoice*npar) ;
    etilda = zeros(numx,nchoice) ;
    j=1 ;
    while j<=nchoice ;
      ztilda(:,npar*(j-1)+1:npar*j) = zmat(:,npar*(j-1)+1:npar*j)+bdisc*fmat(:,numx*(j-1)+1:numx*j)*wz ;
      etilda(:,j) = bdisc * fmat(:,numx*(j-1)+1:numx*j)*we ;      
      j=j+1 ;
    end ;
    clear wz we ;    

%     @ ----------------------------------------------- @
%     @ 4. Sample observations of "ztilda" and "etilda" @
%     @ ----------------------------------------------- @
    zobs = ztilda(indx,:) ;
    eobs = etilda(indx,:) ;

%     @ ----------------------------------------@
%     @ 5. Pseudo Maximum Likelihood Estimation @
%     @ ----------------------------------------@
    [teta0 , var0]=clogit(inda,zobs,eobs,names);    
    tetaest(:,ks) = teta0 ;
    if ks==1
        criter=0;
    else
        criter = max(abs(tetaest(:,ks)-tetaest(:,ks-1))) ;   
    end
    varest(:,1+(ks-1)*npar:ks*npar) = var0 ;
 
%     @ -------------------------- @
%     @ 6. Computing probabilities @
%     @ -------------------------- @
    pini = zeros(numx,nchoice) ;
    j=1 ;
    while j<=nchoice ;
      pini(:,j) = ztilda(:,npar*(j-1)+1:npar*j)*teta0 + etilda(:,j) ;
      j=j+1 ;
    end ;
    pini = pini - max(pini,2) ;
    pini = exp(pini)./repmat(sum(exp(pini),2),1,size(exp(pini),2)) ;
    pest(:,(ks-1)*nchoice+1:ks*nchoice) = pini ;

    ks=ks+1 ;
  end ;

  if (ks<kstage) ;
    disp(' -----------------------------------------------------');
    disp(['CONVERGENCE ACHIEVE AFTER ' num2str(ks) ' NPL ITERATIONS']) ;
    disp(' -----------------------------------------------------');
  end ;
