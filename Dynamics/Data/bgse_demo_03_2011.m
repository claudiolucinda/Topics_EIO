% %// *********************************************************************
% %//  bgse_demo_03_2011.prg
% %//                                                                 
% %//  THIS PROGRAM ESTIMATES A STATIC GAME OF MARKET ENTRY WITH INCOMPLETE INFORMATION 
% %//  USING ACTUAL DATA OF MCDONALDS AND BURGER KING STORE LOCATION DECISIONS
% %//  IN UK LOCAL MARKETS (DATA FROM TOIVANEN AND WATERSON, RAND 2005).
% %//  
% %//  THE ESTIMATION METHODS ARE:
% %//      - TWO-STEP METHOD
% %//      - K-steps
% %//      - NESTED PSEUDO LIKELIHOOD (NPL) 
% %//
% %//  by VICTOR AGUIRREGABIRIA
% %//
% %//  Barcelona, July 5, 2011
% %//
% %// *********************************************************************
% %//
% %// SPECIFICATION OF ONE-PERIOD PROFIT FUNCTION
% %//  The profit function for firm i is:
% %//  
% %//      Ui = zi(ai,aj) * thetai - ai * epsi
% %//
% %//  where ai is the new entry decision of firm i, aj is the
% %//  new entry decision of firm j, zi(ai,aj) are vectors 
% %//  of variables, and thetai is a vector of parameters. More specifically,
% %//
% %//      thetai = (VP0i, VP1i, VP2i, FC1i, FC2i)
% %//
% %//  where VP0i, VP1i, and VP2i are parameters in the variable profit function,
% %//  FC1i and FC2i are parameters in fixed costs. And
% %//
% %//      zi(ai,aj) = { S * 1(xi + ai > 0) }
% %//                ~ { S * (xi + ai - xj - aj) }
% %//                ~ { S * (xi + ai - xj - aj)^2 }
% %//                ~ { -1(xi + ai > 0) }
% %//                ~ { -(xi + ai) }
% %//                ~ { -(xi + ai)^2 }
% %//
% Translated into MATLAB by Claudio Lucinda - University of Sao Paulo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear; clc;
% 
% %// ****************************************
% %// PART 1: SPECIFICATION OF SOME CONSTANTS
% %// ****************************************
% 
% %// Constants of the datafile
% %// Name and address of data file

% filedat = 
% "c:\\mypapers\\arvind_rationalizability\\data\\toivanen_waterson_nolondon_120809.dat";

% Change dir here
wdir='C:\Documents and Settings\Claudio Lucinda\Meus documentos\Pesquisa\Curso Verão 2011\Summer course Barcelona\Datasets and code\';
path(wdir,path);

load([wdir 'Class2.mat']);

nobs = size(data,2) ;       %%// Number of observations in data file
nmarket = 422 ;     %%// Number of local markets
nyear = 5 ;         %%// Number of years
nvar = 27 ;         %%// Number of variables in dataset

%%// Constants of the model
maxstore = 15 ;    %%// maximum possible values number of stores
nplayer = 2 ;      %%// Number of players
maxiter = 40 ;     %%// Maximum number of iterations for NPL algorithm

%%// Vector with names of parameters
namesb1 =  {'VP0_BK';'VP1_BK';'VP2_BK';'FC0_BK';'FC1_BK';'FC2_BK'; ...
        'VP0_MD';'VP1_MD';'VP2_MD';'FC0_MD';'FC1_MD';'FC2_MD'} ;
namesb2 = {'DENSITY';'GDP';'RENT';'TAX'};
namesb = cat(1,namesb1,namesb2) ;

%%// Calculating some constants
vstate = (0:1:maxstore-1)'; %%// Column vector (0,1,2,...,maxstore-1)'
vstate = [kron(vstate,ones(maxstore,1))  kron(ones(maxstore,1),vstate)]; 
%%// Matrix with all possible values of the state variables

nstate = size(vstate,1) ;
kp1 =  size(namesb1,1)/2 ;
kp2 =  size(namesb2,1) ;
kparam = size(namesb,1) ;

% %// ***************************************************
% %// PART 2. CONSTRUCTION OF VARIABLES
% %// ***************************************************

county_name     = textdata(2:end,1) ;
district_name   = textdata(2:end,2) ;
county_code     = data(:,1) ;
district_code   = data(:,2) ;
year            = data(:,3) ;
mcd_stock       = data(:,4) ;
mcd_entry       = data(:,5) ;
mcd_entdum      = data(:,6) ;
bk_stock        = data(:,7) ;
bk_entry        = data(:,8) ;
bk_entdum       = data(:,9) ;
district_area   = data(:,10) ;
population      = data(:,11) ;
pop_0514        = data(:,12) ;
pop_1529        = data(:,13) ;
pop_4559        = data(:,14) ;
pop_6064        = data(:,15) ;
pop_6574        = data(:,16) ;
avg_rent        = data(:,17) ;
ctax            = data(:,18) ;
ecac            = data(:,19) ;
ue              = data(:,20) ;
gdp_pc          = data(:,21) ;
dist_bkhq_miles = data(:,22) ;
dist_bkhq_minu  = data(:,23) ;
dist_mdhq_miles = data(:,24) ;
dist_mdhq_minu  = data(:,25) ;

%%// Construction of variables
x_bk = bk_stock ;       %%// Stock of stores for BK
x_md = mcd_stock ;      %%// Stock of stores for MD
a_bk = (bk_entry>0) ;      %%// Dummy of new entry for BK
a_md = (mcd_entry>0) ;     %%// Dummy of new entry for MD
population = population/1000 ;  %%// Population in millions
density = population./district_area ;

%%// Market specific mean values of some exogenous explanatory variables
marketsize = mean(reshape(population',nyear,nmarket)',2) ;

%marketsize = mean(reshape(population,nyear,nmarket),2) ;
zmarket = [mean(reshape(density',nyear,nmarket)',2) mean(reshape(gdp_pc',nyear,nmarket)',2) ...
    mean(reshape(avg_rent',nyear,nmarket)',2) mean(reshape(ctax',nyear,nmarket)',2)];


% argums.numx=numx;
% argums.numq=numq;
% argums.prob1=prob1; 
% argums.t=t;
% argums.selx=selx;
% argums.denom=denom;
% argums.numer=numer ; 


% @ -------------------------------------------------------------------@
% @                  THE MAIN PROGRAM STARTS HERE                      @
% @ -------------------------------------------------------------------@
% 
% %// ***********************************
% %// STEP 0: ESTIMATION OF INITIAL CCPs
% %// ***********************************
% 
% %// prob_freq is a matrix with 2 columns, one for each player.
% %//  A column contains the frequency estimates of the CCPs for
% %//  a player and for every market and every possible value of xi and xj

prob_freq = zeros(nmarket*nstate,nplayer) ; %// Makes prob_freq equal to zero

%// Calculates fruquency estimates
%// Note that these frequency estimates are very imprecise. 
market = 1 ;
while market<=nmarket ;
  count1 = (market-1)*nstate + 1 ;
  count2 = market*nstate ; 
  yyy_bk = a_bk((market-1)*nyear+1:market*nyear) ;
  yyy_md = a_md((market-1)*nyear+1:market*nyear) ;
  xxx = [x_bk((market-1)*nyear+1:market*nyear) x_md((market-1)*nyear+1:market*nyear)];
  prob_freq(count1:count2,1) = freqprob(yyy_bk,xxx,vstate) ;
  prob_freq(count1:count2,2) = freqprob(yyy_md,xxx,vstate) ;
  market = market+1 ;
end ;

%ponto de partida - valores aleatórios uniformes

prob_freq = rand(nmarket*nstate,nplayer);

%// ************************
%// PART 5: NPL ESTIMATION
%// ************************

argums.namesb=namesb;

[best,varb,pst_pobs]=npl_static([a_bk a_md],[x_bk x_md],marketsize,zmarket,prob_freq,vstate,maxiter,namesb);

