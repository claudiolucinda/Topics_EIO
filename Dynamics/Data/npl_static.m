% %// -------------------------------
% %// C. PROCEDURE for NPL ESTIMATOR 
% %// ------------------------------- 
function [thetaest,varest,pest_obs] = npl_static(yobs, xobs, msize, zmarket, pchoice, mstate, kiter, namesb)

%     %// -----------------------------------------------------------------------
%     %// NPL_STATIC
%     %//      This procedure iterates in the NPL algorithm for a static game. 
%     %//      The algorithm is initialized with a vector of CCPs
%     %//      The procedure returns the vector of parameter estimates, 
%     %//      the variance matrix, and the matrix with players choice 
%     %//      probabilities at every sample point.
%     %//
%     %// FORMAT      (thetaest, varest, pest_obs) =
%     %//                  npl_static(yobs, xobs, msize, zmarket, pchoice, mstate, kiter, namesb)
%     %//
%     %// INPUTS:
%     %//
%     %//      yobs    -   (nobs x 2) matrix with observations of players' choices
%     %//
%     %//      xobs    -   (nobs x 2) matrix  with observations of players' pre-existing
%     %//                  number of stores
%     %//
%     %//      msize   -   (nmarket x 1) vector with observations of market size (population)
%     %//
%     %//      zmarket -   (nmarket x kz) matrix with observations of time-invariant 
%     %//                  market chracteristics
%     %//
%     %//      pchoice -   (nobs x 2) matrix with initial vector of CCPs for every observation 
%     %//                  and player
%     %//
%     %//      mstate  -   (nstate x 2) matrix with all the possible values of the 
%     %//                  endogenous state variables
%     %//
%     %//      kiter   -   Scalar natural number with number of NPL iterations
%     %//
%     %//      namesb  -   (K x 1) vector with names of the structural parameters
%     %//
%     %//  OUTPUTS:
%     %//
%     %//  thetaest    -   (K x1) vector with parameter estimates at the last NPL iteration
%     %//
%     %//  varest      -   (K xK) matrix of variances and covariances
%     %//
%     %//  pest_obs    -   (nobs x 2) matrix with estimates of CCPs for every observation and player
%     %//
%     %// -----------------------------------------------------------------------
% Translation into MATLAB - Claudio Lucinda - University of Sao Paulo

%   local myzero, nobs, nmarket, nyear, nplayer, ns, numx, ktot, kvpfc, kz, indxobs, j, 
%         xbk, xmd, npliter, criterion, conv_const, theta0, eobs,
%         p_bk, p_md, ztilda_bk, ztilda_md,
%         ztilda_obs_bk, ztilda_obs_md,
%         m, valmsize, valzmarket,
%         zbk_00, zbk_01, zbk_10, zbk_11, zmd_00, zmd_01, zmd_10, zmd_11,
%         eprofbk_0, eprofbk_1, eprofmd_0, eprofmd_1,       
%         zt_bk, zt_md, count1, count2,
%         zobs, thetaest, varest, likelihood, theta_bk, theta_md, pest_obs ;        


  %// ---------------
  %// Some constants 
  %// ---------------
  myzero=1e-16;          %// Constant for truncation of CCPs to avoid numerical errors
  nobs = size(yobs,1) ;       %// Total number of market*year observations 
  nmarket = size(msize,1) ;   %// Total number of markets in the sample
  nyear = nobs/nmarket ;    %// Number of years in the sample (balanced panel)
  if isequal(nyear,floor(nyear))==0 ; disp('ERROR: Number of years is not an integer'); return; end; 
  nplayer = size(yobs,2) ;    
  ns = size(pchoice,1)/nmarket ;  %// number of states in a single market
  if isequal(ns,floor(ns))==0 ; disp('ERROR: Number of states in a single market is not an integer'); return; end; 
  numx = sqrt(ns) ;     %// number of values of xbk or xmd
  if isequal(numx,floor(numx))==0 ; disp('ERROR: Number of values of xbk or xmd is not an integer'); return; end; 
  ktot = size(namesb,1) ; %// Total number of parameters
  kz = size(zmarket,2) ;  %// Number of parameters associated with the control variables in zmarket
  kvpfc =  (ktot-kz)/2 ;   %// Number of parameters in var profits and fixed costs for a single firm
  if isequal(kvpfc,floor(kvpfc))==0 ; disp('ERROR: Number of parameters in var profits and fixed costs is not an integer'); return; end; 
  xbk = mstate(:,1) ;   %// vector stock of stores for BK
  xmd = mstate(:,2) ;   %// vector stock of stores for MD

  %// -------------------------------------------
  %// Vector with indexes for the observed state  
  %// -------------------------------------------
  indxobs = zeros(nobs,1) ;
  j=1 ;
  while j<=ns ;
    indxobs = indxobs + j.*prod(+(xobs==repmat(mstate(j,:),size(xobs,1),1)),2);
    j=j+1 ;
  end ;

  %// -------------
  %// NPL algorithm 
  %// ------------- 
  criterion = 1000 ;
  conv_const = 1e-12;
  theta0 = zeros(ktot,1) ;
  npliter=1 ;
  while (npliter<=kiter) && (criterion>conv_const) ;
    disp('------------------------------------------------------------------');
    disp(['NPL ITERATION =' num2str(npliter) ' Criterion =' num2str(criterion)]);
    %// ---------------------------------------------------------
    %// TASK 1:  Computing the matrices ztilda_bk and ztilda_md 
    %//          and the vectors etilda_bk and etilda_md
    %//          for every market and every sample observation
    %// ---------------------------------------------------------
    ztilda_bk = zeros(nmarket*ns,kvpfc+kz) ;
    ztilda_md = zeros(nmarket*ns,kvpfc+kz) ;
    ztilda_obs_bk = zeros(nobs,kvpfc+kz) ;
    ztilda_obs_md = zeros(nobs,kvpfc+kz) ;
    m=1;
    while m<=nmarket ;
      valmsize = msize(m) ;
      valzmarket = zmarket(m,:) ;
      %// -------------------------------------------------------------------- 
      %// Selection of probabilities for the market and
      %// truncation of probabilities to avoid inverse Mill's ratio = +INF          
      %// --------------------------------------------------------------------
      p_bk = pchoice((m-1)*ns+1:m*ns,1) ;
      p_md = pchoice((m-1)*ns+1:m*ns,2) ;
      p_bk = (p_bk<=myzero).*myzero+(p_bk>=(1-myzero)).*(1-myzero)+(p_bk>=myzero).*(p_bk<=(1-myzero)).*p_bk ;
      p_md = (p_md<=myzero).*myzero+(p_md>=(1-myzero)).*(1-myzero)+(p_md>=myzero).*(p_md<=(1-myzero)).*p_md ;

      %// ------------------------------ 
      %// Vectors of expected profits 
      %// ------------------------------ 
      zbk_00 = [(valmsize.*(xbk>0)) (valmsize.*(xbk-xmd)) (valmsize.*(xbk-xmd).*(xbk-xmd)) (-(xbk>0)) (-xbk) (-xbk.*xbk) (xbk*valzmarket)];
      zbk_01 = [(valmsize.*(xbk>0)) (valmsize.*(xbk-xmd-1)) (valmsize.*(xbk-xmd-1).*(xbk-xmd-1)) (-(xbk>0)) (-xbk) (-xbk.*xbk) (xbk*valzmarket)];
      zbk_10 = [(valmsize.*((xbk+1)>0)) (valmsize.*(xbk+1-xmd)) (valmsize.*(xbk+1-xmd).*(xbk+1-xmd)) (-((xbk+1)>0)) (-(xbk+1)) (-(xbk+1).*(xbk+1)) ((xbk+1)*valzmarket)];
      zbk_11 = [(valmsize.*((xbk+1)>0)) (valmsize.*(xbk+1-xmd-1)) (valmsize.*(xbk+1-xmd-1).*(xbk+1-xmd-1)) (-((xbk+1)>0)) (-(xbk+1)) (-(xbk+1).*(xbk+1)) ((xbk+1)*valzmarket)];
      zmd_00 = [(valmsize.*(xmd>0)) (valmsize.*(xmd-xbk)) (valmsize.*(xmd-xbk).*(xmd-xbk)) (-(xmd>0)) (-xmd) (-xmd.*xmd) (xmd*valzmarket)];
      zmd_01 = [(valmsize.*(xmd>0)) (valmsize.*(xmd-xbk-1))  (valmsize.*(xmd-xbk-1).*(xmd-xbk-1)) (-(xmd>0)) (-xmd) (-xmd.*xmd) (xmd*valzmarket)];
      zmd_10 = [(valmsize.*((xmd+1)>0)) (valmsize.*(xmd+1-xbk)) (valmsize.*(xmd+1-xbk).*(xmd+1-xbk)) (-((xmd+1)>0)) (-(xmd+1)) (-(xmd+1).*(xmd+1)) ((xmd+1)*valzmarket)];
      zmd_11 = [(valmsize.*((xmd+1)>0)) (valmsize.*(xmd+1-xbk-1)) (valmsize.*(xmd+1-xbk-1).*(xmd+1-xbk-1)) (-((xmd+1)>0)) (-(xmd+1)) (-(xmd+1).*(xmd+1)) ((xmd+1)*valzmarket)];

      eprofbk_0 = (1-repmat(p_md,1,size(zbk_00,2))).*zbk_00 + repmat(p_md,1,size(zbk_01,2)).*zbk_01 ;     %// Expected Profit BK if a=0
      eprofbk_1 = (1-repmat(p_md,1,size(zbk_01,2))).*zbk_10 + repmat(p_md,1,size(zbk_01,2)).*zbk_11 ;     %// Expected Profit BK if a=1
      eprofmd_0 = (1-repmat(p_bk,1,size(zmd_00,2))).*zmd_00 + repmat(p_bk,1,size(zmd_01,2)).*zmd_01 ;     %// Expected Profit MD if a=0
      eprofmd_1 = (1-repmat(p_bk,1,size(zmd_10,2))).*zmd_10 + repmat(p_bk,1,size(zmd_11,2)).*zmd_11 ;     %// Expected Profit MD if a=1

      %// -------------------------------------------------
      %// ztilda_bk, ztilda_md for every possible state
      %// -------------------------------------------------
      zt_bk = eprofbk_1 - eprofbk_0 ;
      zt_md = eprofmd_1 - eprofmd_0 ;

      %// ------------
      %// Filling 
      %// ------------
      count1 = (m-1)*ns + 1 ;
      count2 = m*ns ;
      ztilda_bk(count1:count2,:) = zt_bk ;
      ztilda_md(count1:count2,:) = zt_md ;

      count1 = (m-1)*nyear + 1 ;
      count2 = m*nyear ;
      ztilda_obs_bk(count1:count2,:) = zt_bk(indxobs(count1:count2),:) ;
      ztilda_obs_md(count1:count2,:) = zt_md(indxobs(count1:count2),:) ;
      
      m=m+1;
    end ;
  
    %// ---------------------------------------------
    %// TASK 2: Pseudo Maximum Likelihood Estimation 
    %// ---------------------------------------------
    zobs = [[ztilda_obs_bk(:,1:kvpfc);zeros(nobs,kvpfc)] [zeros(nobs,kvpfc);ztilda_obs_md(:,1:kvpfc)] [ztilda_obs_bk(:,kvpfc+1:kvpfc+kz);ztilda_obs_md(:,kvpfc+1:kvpfc+kz)]] ;
    eobs = zeros(nobs*nplayer,1) ;
    [thetaest,varest,~]= miprobit([yobs(:,1);yobs(:,2)],zobs,eobs,zeros(ktot,1),namesb,1) ;

    %// ---------------------------------------------------
    %// TASK 3: Updating Conditional Choice Probabilities
    %// ---------------------------------------------------
    theta_bk = [thetaest(1:kvpfc);thetaest(2*kvpfc+1:ktot)];
    theta_md = [thetaest(kvpfc+1:2*kvpfc);thetaest(2*kvpfc+1:ktot)];
    pchoice  = [normcdf(ztilda_bk*theta_bk,0,1) normcdf(ztilda_md*theta_md,0,1)] ;

    %// --------------------------
    %// Checking for Convergence
    %// --------------------------
    criterion = max(abs(thetaest-theta0)) ;
    
    theta0 = thetaest ;    
    npliter = npliter+1 ;
  end ;
  
  npliter = npliter-1 ;
  if npliter<=kiter ;
    disp('------------------------------------------------------------------');
    disp(' NPL ALGORITHM HAS CONVERGED TO AN NPL FIXED POINT AFTER');
    disp(['   ' num2str(npliter-1) ' ITERATIONS']) ;
    disp('------------------------------------------------------------------');
    
  end ;
  
  %// ---------------------------------------------
  %//  Observed Conditional Choice Probabilities: 
  %// ---------------------------------------------
  pest_obs =[normcdf(ztilda_obs_bk * theta_bk,0,1) normcdf(ztilda_obs_md * theta_md)];
  
