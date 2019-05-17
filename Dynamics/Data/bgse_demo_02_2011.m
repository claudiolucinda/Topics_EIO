clear; clc;


% // ******************************************************************
% //  STRUCTURAL ECONOMETRICS FOR EMPIRICAL INDUSTRIAL ORGANIZATION
% //
% //  BARCERLONA MICROECONOMETRICS SUMMER SCHOOL
% //
% //      DEMO 2: 
% //          - SOLUTION OF STATIC MODEL OF OLIGOPOLY COMPETITION WITH ENDOGENOUS ENTRY
% //          - GENERATING SIMULATED DATASET FROM THE MODEL
% //
% //  Victor Aguirregabiria
% //  University of TorontO
% //
% //  July 4, 2011
% //
% // ******************************************************************
% Translation from GAUSS to MATLAB - Claudio Lucinda - University of Sao
% Paulo


% @ -------------- @
% @  OUTPUT FILE   @
% @ -------------- @  
% Change dir here
wdir='C:\Documents and Settings\Claudio Lucinda\Meus documentos\Pesquisa\Curso Verão 2011\Summer course Barcelona\Datasets and code';
path(wdir,path);

diary([wdir 'bgse_demo_02_2011.out']);

% @ --------------------------------- @
% @   PARAMETERS SIMULATED DATASET    @
% @ --------------------------------- @  
% // Number of markets and years 
numm = 200 ;        %// Number of geographic markets 
first_year = 1998 ; %// First year in the simuladataset
last_year = 2007 ;  %// Last year in the dataset
numt = last_year - first_year + 1 ; %// Number of years

%// Maximum number of firms 
maxnfirm = 6 ; %// Maximum number for computations

%// Seed for simulations
randn('seed',5333799);
rand('seed',5333799);

% @ ----------------------------------------------------------- @
% @   PARAMETERS IN STOCHASTIC PROCESSES OF EXOGENOUS VARIABLES @
% @ ----------------------------------------------------------- @  
% // Parameters for stochastic processes, AR(1), of Observable Exogenous Variables
rho_lpop = 0.95 ;  %// Autorregressive parameter log(population)
rho_linc = 0.90 ;  %// Autorregressive parameter log(income p.c.)
rho_lpin = 0.7 ;   %// Autorregressive parameter log(input price)
m_lpop = 0.2 ; %// Mean of log(Population), where population is in thousands
s_lpop = 0.05 ; %// Std. dev. of log(Population), where population is in thousands
m_linc = 0.1 ; %// Mean of log(income p.c.), where income p.c. is in thousands
s_linc = 0.05 ; %// Std. dev. of log(income p.c.), where income p.c. is in thousands
m_lpin = 0 ; %// Mean of log(input price), where price is in $ per ton
s_lpin = 0.1 ; %// Std. dev. of log(input price), where price is in $ per ton

%// Distribution of Unobservable Exogenous Variables 
rho_dem = 0.8 ;  %// Autorregressive parameter demand shock
rho_mc = 0.9 ;   %// Autorregressive parameter marginal cost
rho_fc = 0.0 ;   %// Autorregressive parameter fixed cost
su_dem = 0.1 ;   %// Std. dev. of innovation in demand error
su_mc = 0.1 ;    %// Std. dev. of innovation in marginal cost
su_fc = log(2.0) ;   %// Std. dev. of innovation in fixed cost

% @ ----------------------------------------- @
% @   PARAMETERS IN DEMAND and COST FUNCTIONS @
% @ ----------------------------------------- @  

%// Parameters in Inverse Demand
ad_0 = log(5000) ;   %// Intercept parameter in log-log inverse demand
ad_pop = 0.20 ;     %// Coefficient of log population
ad_inc = 0.10 ;     %// Coefficient of log income
ad_q = 0.37 ;       %// Coefficient of log output (demand elasticity = 1/ad_q)
su_ad_q = 0.01 ;    %// Std. dev. of random coefficient in log output

%// Parameters in Marginal Cost
amc_0 = log(50) ;   %// exp(amc_0) is the marginal cost (in $ per ton) for the benchmark market
amc_pin = 0.7 ;    %// Coefficient of log input price in log-log equation for marginal cost

%// Parameters in Fixed Cost
afc_0 = log(6e5) ;  %// exp(afc_0) is the fixed cost in $ for the benchmark market
afc_pop = 0 ;      %// Coefficient of log population in log-log eq. for fixed cost
afc_inc = 0 ;      %// Coefficient of log income in log-log eq. for fixed cost

% @ ------------------------------------------------ @
% @ ****************   MAIN PROGRAM   ************** @
% @ ------------------------------------------------ @  

% @ ----------------------------------------------------------------- - @
% @   GENERATING MATRICES WITH MARKET CODE and YEAR OF EACH OBSERVATION @
% @ ----------------------------------------------------------------- - @  

market=1:numm'*ones(1,numt);
%    // seqa(1,1,numm) generates a column vector (1,2,3,...,numm)'
%    // ones(1,numt) is row vector of ones (1,1,...1)
%    // .* is the element-by-element product
%    // Therefore, market is a matrix (numm x numt) where each row contains
%     //  the code of the market
%     //
%     //          market = (  1   1   1       ... 1
%     //                      2   2   2       ... 2
%     //                      ....
%     //                      numm numm numm ... numm )

year=(first_year:1:last_year'*ones(1,numm))';
%     // year is a matrix (numm x numt) with the year of each observation
%     //
%     //          year = (  1998   1999   2000       ... 2007
%     //                    1998   1999   2000       ... 2007
%     //                      ....
%     //                    1998   1999   2000       ... 2007 )
% 
% @ ---------------------------------------------------------------- @
% @   SIMULATING DATA for OBSERVABLE EXOGENOUS EXPLANATORY VARIABLES @
% @ ---------------------------------------------------------------- @  
lpop = zeros(numm,numt) ;   %// Initializing lpop with matrix of zeroes
linc = zeros(numm,numt) ;   %// Initializing linc with matrix of zeroes
lpin = zeros(numm,numt) ;   %// Initializing lpop with matrix of zeroes

% // Initial conditions for each market 
% //  These initial conditions are random draws from the stationary distribution
% //      of each variable
lpop(:,1) = m_lpop/(1-rho_lpop) ...
    + (s_lpop/sqrt(1-rho_lpop*rho_lpop)) * randn(numm,1) ; 
linc(:,1) = m_linc/(1-rho_linc) ...
    + (s_linc/sqrt(1-rho_linc*rho_linc)) * randn(numm,1) ;
lpin(:,1) = m_lpin/(1-rho_lpin) ...
    + (s_lpin/sqrt(1-rho_lpin*rho_lpin)) * randn(numm,1) ;

%// Recursive simulation of the exogenos explanatory variables
t=2 ;
while t<=numt ;
  lpop(:,t) = m_lpop + rho_lpop*lpop(:,t-1) + s_lpop*randn(numm,1) ;
  linc(:,t) = m_linc + rho_linc*linc(:,t-1) + s_linc*randn(numm,1) ;
  lpin(:,t) = m_lpin + rho_lpin*lpin(:,t-1) + s_lpin*randn(numm,1) ;
  t=t+1 ;
end ;

% // Variables in deviations with respect to mean
devlpop = lpop - mean(mean(lpop)) ;
devlinc = linc - mean(mean(linc)) ;
devlpin = lpin - mean(mean(lpin)) ;

% @ ------------------------------------------------------------------ @
% @   SIMULATING DATA for UNOBSERVABLE EXOGENOUS EXPLANATORY VARIABLES @
% @ ------------------------------------------------------------------ @  
edem = zeros(numm,numt) ;   %// Initializing edem with matrix of zeroes
emc = zeros(numm,numt) ;    %// Initializing emc with matrix of zeroes
efc = zeros(numm,numt) ;    %// Initializing efc with matrix of zeroes
ead_q = zeros(numm,numt) ;  %// Initializing ead_q with matrix of zeroes

%// Initial conditions for each market 
%//  These initial conditions are random draws from the stationary distribution
%//      of each variable
edem(:,1) = (su_dem/sqrt(1-rho_dem*rho_dem)) * randn(numm,1) ;
emc(:,1)  = (su_mc/sqrt(1-rho_mc*rho_mc)) * randn(numm,1) ;
efc(:,1)  = (su_fc/sqrt(1-rho_fc*rho_fc)) * randn(numm,1) ;

%// Recursive simulation of the exogenos explanatory variables
t=2 ;
while t<=numt ;
  edem(:,t) = rho_dem*edem(:,t-1) + su_dem*randn(numm,1) ;
  emc(:,t)= rho_mc*emc(:,t-1)+ su_mc*randn(numm,1) ;
  efc(:,t)= rho_fc*efc(:,t-1)+ su_fc*randn(numm,1) ;
  t=t+1 ;
end ;
ead_q  = su_ad_q * randn(numm,numt) ;

% @ ----------------------------------------------------- @
% @   SIMULATING EXOGENOUS COMPONENTS OF DEMAND and COSTS @
% @ ----------------------------------------------------- @  
% // Demand
ademand = ad_0 + ad_pop*devlpop + ad_inc*devlinc + edem ;

%// Marginal cost
mc = exp(amc_0 + amc_pin*devlpin + emc) ;

%// Fixed cost
fixedc = exp(afc_0 + afc_pop*devlpop + afc_inc*devlinc + efc) ;


argums.mc=mc;
argums.ad_q=ad_q;
argums.maxnfirm=maxnfirm;
argums.ademand=ademand;
argums.ead_q=ead_q;
argums.fixedc=fixedc;
argums.numm=numm;

% @ ---------------------------------------------------------- @
% @   SIMULATING EQUILIBRIUM NUMBER OF FIRMS, PRICE and OUTPUT @
% @ ---------------------------------------------------------- @  
nplants = zeros(numm,numt) ;    %// Initializing nplants with matrix of zeroes
quantity = zeros(numm,numt) ;   %// Initializing quantity with matrix of zeroes
price = zeros(numm,numt) ;      %// Initializing price with matrix of zeroes

vecnplant = (0:1:maxnfirm+1)' ;
value0 = zeros(numm,maxnfirm) ;


t=1 ;
while t<=numt ;
  [cournot_q, cournot_p, cournot_profit] = cournot_equil(t,argums) ;
  value0 = cournot_profit ;
  seldum = zeros(numm,maxnfirm+1) ;
%// Obtains the equilibrium number of firms
  n=0 ;
  while (n<=maxnfirm) ;
    if (n==0) ;
      seldum(:,n+1)= (value0(:,1)<0) ;
    elseif (n>0) && (n<maxnfirm) ;
      seldum(:,n+1) = (value0(:,n)>=0).*(value0(:,n+1)<0) ;
    elseif (n==maxnfirm) ;
      seldum(:,n+1)= (value0(:,maxnfirm)>=0) ;
    end;
    n=n+1 ;
  end ;

  %// test
  flags = sum(seldum') ;
  if max(flags)>1 ;
    disp('ERROR: seldum columns do not sum to 1') ; 
  break;
  end ;

  %// Obtains the equilibrium number of firms, quantity, and price
  nplants(:,t) = sum(seldum*vecnplant(2:end),2) ;
  quantity(:,t) = sum(seldum(:,2:maxnfirm+1).*cournot_q,2) ;
  price(:,t)= sum(seldum(:,2:maxnfirm+1).*cournot_p,2) ;
  t=t+1 ;
end ;

hist(reshape(nplants,numm*numt,1),100);
title('HISTOGRAM: NUMBER OF PLANTS IN LOCAL MARKET') ; 
pause(5) ;

hist(reshape(price,numm*numt,1),100);
title('HISTOGRAM: LOCAL MARKET PRICE') ;
pause(5) ;

hist(reshape(quantity,numm*numt,1),100);
title('HISTOGRAM: LOCAL MARKET OUTPUT') ;
pause(5) ;



% @ --------------------------------------------------- @
% @   SAVING DATA  (with variables reshaped as vectors) @
% @ --------------------------------------------------- @
market  = reshape(market,[],1) ;
year    = reshape(year,[],1) ;
popu    = reshape(exp(lpop),[],1) ;
pcincome= reshape(exp(linc),[],1) ;
quantity= reshape(quantity,[],1) ;
price   = reshape(price,[],1) ;
pinput  = reshape(exp(lpin),[],1) ;


% @ -------------------- @
% @   DEMAND ESTIMATION  @
% @ -------------------- @
lpop = log(popu) ;
linc = log(pcincome) ;
lpin = log(pinput) ;
lquan = log(quantity) ;
lprice = log(price) ;

% "------------" ;
% "2SLS: Step 1" ;
% "------------" ;
%__altnam = "logPOP"|"logINCOME"|"logPINPUT"|"logPRICE" ;
yobs = lprice ;
yobs = yobs(quantity>0) ;
xobs = [ones(size(lpop)) lpop linc lpin];
xobs = xobs(quantity>0,:) ;
beta=regress(yobs,xobs);
lpricehat = xobs*beta;

% "------------" ;
% "2SLS: Step 2" ;
% "------------" ;
yobs = lquan ;
yobs = yobs(quantity>0) ;
xobs = [ones(size(lpop)) lpop linc];
xobs = [xobs(quantity>0,:) lpricehat];
beta2=regress(yobs,xobs);


diary off ;

