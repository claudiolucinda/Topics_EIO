function [best,varb,likelihood]=npldygam(yobs,yobs_1,pchoice,disfact,mstate,b0,kiter,namesb);
%// ----------------------------------
%// 10. PROCEDURE for NPL ITERATIONS
%// ----------------------------------

%// ---------------
  %// Some constants 
  %// ---------------
  myzero = 1e-16 ;
  nobs = size(yobs,1) ;
  nplayer = size(yobs,2) ;
  numx = size(pchoice,1) ;
  kparam = size(b0,1) ;
  best = zeros(kparam,kiter) ;
  varb = zeros(kparam,kparam*kiter) ; 
  yobs = [yobs(:,1);yobs(:,2)];
  nstate = size(pchoice,1) ;    %%// number of states
  
  
  %// --------------------------------------------
  %// Vector with indexes for the observed state  
  %// --------------------------------------------
  %repmat(vstate(1,:),size(aobs_1,1),1)),2)
  indobs = 1.*prod(+(yobs_1==repmat(mstate(1,:),size(yobs_1,1),1)),2)+ 2.*prod(+(yobs_1==repmat(mstate(2,:),size(yobs_1,1),1)),2)+ ...
      3.*prod(+(yobs_1==repmat(mstate(3,:),size(yobs_1,1),1)),2)+ 4.*prod(+(yobs_1==repmat(mstate(4,:),size(yobs_1,1),1)),2);
  
  %// ------------- 
  %// NPL algorithm 
  %// ------------- 
  cconv = 1e-6 ;    %// convergence constant
  criter = 1000 ;   %//  convergence criterion
  pchoice_0 =  pchoice ;   %// Initial vector of CCPs
  iter=1 ;
  while (iter<=kiter) && (criter>cconv) ;
    %// ---------------------------------------- 
    %// a. Truncation of probabilities to avoid  
    %//    inverse Mill's ratio = +INF           
    %// ---------------------------------------- 
    pchoice_0(:,1) = (pchoice_0(:,1)<myzero).*myzero ...
        + (pchoice_0(:,1)>(1-myzero)).*(1-myzero)+ ...
        (pchoice_0(:,1)>=myzero).*(pchoice_0(:,1)<=(1-myzero)).*pchoice_0(:,1) ;
    pchoice_0(:,2) = (pchoice_0(:,2)<myzero).*myzero ...
        + (pchoice_0(:,2)>(1-myzero)).*(1-myzero)+ ...
        (pchoice_0(:,2)>=myzero).*(pchoice_0(:,2)<=(1-myzero)).*pchoice_0(:,2);
    
    %// -------------------------------------
    %// b. Matrix of transition probabilities
    %// ------------------------------------- 
    ptran = [(1-pchoice_0(:,1)).*(1-pchoice_0(:,2)) ...
        (1-pchoice_0(:,1)).*pchoice_0(:,2) ...
        pchoice_0(:,1).*(1-pchoice_0(:,2))  ...
        pchoice_0(:,1).*pchoice_0(:,2)];
    
    %// --------------------
    %// c. Inverse of I-b*F 
    %// --------------------
    %inv_bf = inv(eye(numx)-disfact*ptran) ;        
    
    %// ------------------------------------
    %// d. Matrices Pr(a[t) | a[t-1), ai[t))
    %// ------------------------------------
    iptran1_a0 = [(1-pchoice_0(:,2))  pchoice_0(:,2) zeros(nstate,1) zeros(nstate,1)];
    iptran1_a1 = [zeros(nstate,1) zeros(nstate,1) (1-pchoice_0(:,2)) pchoice_0(:,2)];    
    iptran2_a0 = [(1-pchoice_0(:,1)) zeros(nstate,1)  pchoice_0(:,1) zeros(nstate,1)];
    iptran2_a1 = [zeros(nstate,1) (1-pchoice_0(:,1)) zeros(nstate,1) pchoice_0(:,1)];
    
    %// -----------------------------------------
    %// e. Construction of explanatory variables 
    %// -----------------------------------------
    umat1_a0 = zeros(numx,kparam) ;
    umat1_a1 = [(-(1-mstate(:,1))) (1-pchoice_0(:,2)) pchoice_0(:,2)];
    umat2_a0 = zeros(numx,kparam) ;
    umat2_a1 = [(-(1-mstate(:,2))) (1-pchoice_0(:,1)) pchoice_0(:,1)];
    
    zmat1 = repmat((1-pchoice_0(:,1)),1,size(umat1_a0,2)).*umat1_a0 + repmat(pchoice_0(:,1),1,size(umat1_a1,2)).*umat1_a1 ;
    zmat1 = (eye(numx)-disfact*ptran)\zmat1 ;
    zmat1 = (umat1_a1 + disfact * iptran1_a1 * zmat1)...
          - (umat1_a0 + disfact * iptran1_a0 * zmat1) ;
          
    zmat2 = repmat((1-pchoice_0(:,2)),1,size(umat2_a0,2)).*umat2_a0 + repmat(pchoice_0(:,1),1,size(umat2_a1,2)).*umat2_a1 ;
    zmat2 = (eye(numx)-disfact*ptran)\zmat2 ;
    zmat2 = (umat2_a1 + disfact * iptran2_a1 * zmat2)...
          - (umat2_a0 + disfact * iptran2_a0 * zmat2) ;
    
    emat1 = normpdf(norminv(pchoice_0(:,1),0,1),0,1) ;
    emat1 = (eye(numx)-disfact*ptran)\emat1 ;
    emat1 = (disfact * iptran1_a1 * emat1)...
          - (disfact * iptran1_a0 * emat1) ;
          
    emat2 = normpdf(norminv(pchoice_0(:,2),0,1),0,1) ;
    emat2 = (eye(numx)-disfact*ptran)\emat2 ;
    emat2 = (disfact * iptran2_a1 * emat2)...
          - (disfact * iptran2_a0 * emat2) ;
    
    zobs = [zmat1(indobs,:);zmat2(indobs,:)] ;
    eobs = [emat1(indobs,:);emat2(indobs,:)];
    namesb = {'Entry Cost';'Monopoly Prof';'Duopoly Prof'}; %%// Vector with names of parameters
        
    %// ----------------------------------------
    %// f. Pseudo Maximum Likelihood Estimation 
    %// ----------------------------------------
    [tetaest,varest,likelihood]= miprobit(yobs,zobs,eobs,zeros(kparam,1),namesb,0) ;
    best(:,iter) = tetaest ;
    varb(:,(iter-1)*kparam+1:iter*kparam) = varest ;
    sdb    = sqrt(diag(varest)) ;
    tstat  = tetaest./sdb ;
               
    %// ------------------------- 
    %// g. Updating probabilities 
    %// ------------------------- 
    pchoice_1 = [normcdf(zmat1*tetaest +emat1,0,1) ... 
        normcdf(zmat2*tetaest +emat2,0,1)];
        
    %// -----------------------------
    %// h. Checking for Convergence 
    %// -----------------------------
    if (iter==1) ;
      criter = 1000 ;
    elseif (iter>1)
      criter = max(max(abs(best(:,iter)-best(:,iter-1)))) ;
    end;

    %// -------------------------------------------------------------
    %// i. Presenting estimaton results from current NPL iteration
    %// -------------------------------------------------------------
    
    disp('------------------------------------------------------------------------');
    disp(['       NPL ITERATION NUMBER        = ' num2str(iter)]); 
    disp(['       NPL Convergence Criterion   = ' num2str(criter)]) ;
    disp('       Estimation Results from Current Iteration');
    disp('------------------------------------------------------------------------');
    disp(['       Log-Likelihood function  = ' num2str(likelihood)]);
    disp('------------------------------------------------------------------------');
    disp('       Parameter     Estimate        Standard        t-ratios');
    disp('                                     Errors') ;
    disp('------------------------------------------------------------------');
    k=1 ;
    while k<=kparam ;
      disp([namesb(k) num2str(best(k)) num2str(sdb(k)) num2str(tstat(k))]);
      k=k+1 ;
    end;
    disp('------------------------------------------------------------------');
    
  
    pchoice_0 = pchoice_1 ; 
    iter=iter+1 ;
  end
    disp('------------------------------------------------------------------');
    disp('------------------------------------------------------------------');
    disp('   NPL ESTIMATION'); 
    disp(['       CONVERGENCE ACHIEVED AFTER ' num2str(iter-1) ' NPL ITERATIONS']);
    disp('------------------------------------------------------------------');
    disp('------------------------------------------------------------------');
    disp('Note:  Parameters are idetified up to scale.');
    disp('       More specifically, the reported values are estimates');
    disp('       of the profit parameters divided by sqrt(2)*sigma,') ;
    disp('       where sigma is the standard deviation of eps(0) and eps(1)');
    disp('------------------------------------------------------------------');
    disp('------------------------------------------------------------------');
    
  