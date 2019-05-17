
% %//  ****************************************************************************
% %//
% %//  bgse_demo_05_2011.prg
% %//
% %//  Program for the estimation of a simple dynamic game
% %//  of entry-exit with 2 firms
% %//
% %//  Given parameters fixed by user, the program:
% %//      (a) computes an equilibrium (Markov Perfect equilibrium) of the model
% %//      (b) generates an artifitial / simulated dataset from that equilibrium
% %//      (c) uses the simulated as actual data and estimates the parameters of the model
% %//
% %//  The estimation methods are:
% %//      (a) Two-step Pseudo Maximum Likelihood
% %//      (b) Iterated Pseudo Maximum Likelihood
% %//
% %//  by Victor Aguirregabiria
% %//
% %//  Barcelona, July 2011
% %//
% %//  ****************************************************************************


% %// --------------
% %//  OUTPUT FILE  
% %// --------------
wdir='C:\Documents and Settings\Claudio Lucinda\Meus documentos\Pesquisa\Curso Verão 2011\Summer course Barcelona\Datasets and code\';
path(wdir,path);

diary([wdir 'mcarlo_psd_1207.out']);

% %// ------------------------------------------------------------------
% %//  Specification of Profit Function
% %//
% %//  Action firm i           = a_i
% %//  Action of competiror    = a_j
% %//
% %//      a_i = 0 ----> No active in market
% %//      a_i = 1 ----> Active in market
% %//
% %// ------------------------------------------------------------------
% %//
% %//  The game is dynamic because there is a sunk cost of entry
% %//
% %//  The state variables of this game are the indicators of whether
% %//  the firms were active or not in the market at previous period.
% %//  These are payoff relevant state variables because they determine
% %//  whether a firm has to pay an entry cost to operate in the market.
% %//
% %//          x_i = a_i[t-1)
% %//
% %//          x_i = 0 ----> No active in market at previous period
% %//              = 1 ----> Active in market at previous period
% %//
% %// ------------------------------------------------------------------
% %//
% %//  Parameters:
% %//      ce  =   Entry cost
% %//      pi1 =   Operating profit of a monopolist
% %//      pi2 =   Operating profit of a duopolist
% %//
% %// ------------------------------------------------------------------
% %//
% %//  Profit function:
% %//
% %//      Profit_i(a_i = 0) = eps_i(0)
% %//
% %//      Profit_i(a_i = 1) = (1-aj) * pi1 + aj * pi1 
% %//
% %//                       - (1-a_i[t-1)) * ce + eps_i(1)
% %//
% %//  where eps_i(0) and eps_i(1) are private information shocks that are
% %//  i.i.d. over time and over players, and independent of each other,
% %//  with distribution Normal( 0 , ssigma^2)
% %//
% %// ------------------------------------------------------------------
% 
% %// -----------------------------------------------
% %//  1. Values of parameters and other constants
% %// -----------------------------------------------
ce  = 1.0 ;    %%// Entry cost
pi1 = 1.0 ;     %%// Monopoly  profit
pi2 = 0.4 ;     %%// Duopoly profit

dfact   = 0.9 ;    %%// Discount factor
ssigma   = 1.0 ;     %%// Std. Dev. of eps(0) and eps(1) @

nplayer = 2 ;       %%// Number of players
nobs    = 1000 ;    %%// Number of observations in simulated dataset
npliter = 20 ;      %%// Maximum number of NPL iterations

trueparam = [ce;pi1;pi2];  %%// vector with true values of parameters
namesb = {'Entry Cost';'Monopoly Prof';'Duopoly Prof'}; %%// Vector with names of parameters
kparam  = size(trueparam,1) ;   %%//  Number of parameters to estimate

% %// 'vstate' is a matrix with all the possible values of
% %//  the state variables of this dynamic game.
% %//  That is, a matrix with all the possible values of x_i and x_j
% %//
% %//  - Each row represents a value for the state variables
% %//  - Column 1 contains the values of x_i, and column 2 the values of x_j

vstate = [[0 0];[0 1];[1 0];[1 1]] ;
nstate = size(vstate,1) ; %%// Number of possible values of the vector of state variables

randn('seed',5333799);
rand('seed',5333799);




%// -----------------------------------------------------------------------
%//  3. COMPUTING A MARKOV PERFECT EQUILIBRIUM OF THIS DYNAMIC GAME
%// -----------------------------------------------------------------------
prob0 = rand(nstate,nplayer) ;

disp('************************************************************************');
disp('************************************************************************');
disp('   COMPUTING A MPE OF THE DYNAMIC GAME');
disp('************************************************************************');
disp('************************************************************************');

disp('------------------------------------------------------------------------');
disp('       Values of the structural parameters');
disp(['                   Entry cost  = ' num2str(ce)]);
disp(['               Monopoly profit = ' num2str(pi1)]);
disp(['                Duopoly profit = ' num2str(pi2)]);
disp(['               Discount factor = ' num2str(dfact)]);
disp('------------------------------------------------------------------------');
disp('       BEST RESPONSE MAPPING ITERATIONS');
cconv = 1e-12 ;
criter = 1000 ;
maxiter = 100 ;
iter=1 ;
argums.pi1=pi1;
argums.pi2=pi2;
argums.ce=ce;
argums.vstate=vstate;
argums.ssigma=ssigma;
argums.dfact=dfact;

while (criter>cconv) && (iter<=maxiter) ;
  disp(['         Best response mapping iteration  = ' num2str(iter)]);
  disp(['         Convergence criterion            = ' num2str(criter)]);
  prob1 = equilmap(prob0,argums);
  criter = max(max(abs(prob1-prob0))) ;
  prob0 = prob1 ;
  iter=iter+1 ;
end;
pequil = prob0 ;

if (iter<maxiter) ;
disp('------------------------------------------------------------------------');
disp(['         CONVERGENCE ACHIEVED AFTER ' num2str(iter) ' BEST RESPONSE ITERATIONS']);
disp('------------------------------------------------------------------------');
disp('         EQUILIBRIUM PROBABILITIES');
  disp(pequil)
  disp('     Remember that');
  disp('     vstate  = (0 ~ 0)');
  disp('             | (0 ~ 1)');
  disp('             | (1 ~ 0)');
  disp('             | (1 ~ 1)');
disp('------------------------------------------------------------------------');
end ;


%// --------------------------------------------------------------------
%// 4. COMPUTING THE STEADY-STATE DISTRIBUTION OF THE STATE VARIABLES
%//      for THE COMPUTED EQUILIBRIUM
%//
%//      I will use this distribution to draw the initial value of the
%//      vector of state variables in the simulated sample
%// --------------------------------------------------------------------

disp('************************************************************************');
disp('************************************************************************');
disp('   COMPUTING STEADY-STATE OR ERGODIC DISTRIBUTION OF THE STATE VARIABLES');
disp('************************************************************************');
disp('************************************************************************');

%// Transition probability matrix for the state variables
ftran = [(1-pequil(:,1)).*(1-pequil(:,2)) ...
    (1-pequil(:,1)).*pequil(:,2)  pequil(:,1).*(1-pequil(:,2)) ...
    pequil(:,1).*pequil(:,2)];

%// By definition, the steady-state distribution 'psteady' 
%//  is such that:
%//
%//      psteady = Ftran * psteady
%//
%//  where Ftran is the transition probability matrix of the state vars

cconv = 1e-6 ;
criter = 1000 ;
psteady = (1/nstate)*ones(nstate,1) ;
while criter>cconv ;
  disp(['Criter = ' num2str(criter)]);
  pbuff = ftran'*psteady ;
  criter = max(abs(pbuff-psteady)) ;
  psteady = pbuff ;
end;

disp('------------------------------------------------------------------------');
disp('       Ergodic Distribution of the State Variables');
disp(psteady)

disp('------------------------------------------------------------------------');
disp('     Remember that');
  disp('     vstate  = (0 ~ 0)');
  disp('             | (0 ~ 1)');
  disp('             | (1 ~ 0)');
  disp('             | (1 ~ 1)');
disp('------------------------------------------------------------------------');



%// ------------------------
%//  6. SIMULATING DATA
%// ------------------------
[aobs , aobs_1]= sindygam(nobs,pequil,psteady,vstate) ;


%// ----------------------------------------
%//  8.  FREQUENCY ESTIMATES
%// ----------------------------------------
freq_est_prob = freqprob(aobs,aobs_1,vstate) ; 

disp('************************************************************************');
disp('************************************************************************');
disp('   NON-PARAMETRIC ESTIMATES OF CCPS AND DISTRIBUTION OF STATE VARIABLES ');
disp('   USING SIMULATED DATA FROM THE COMPUTED MPE ');
disp('************************************************************************');
disp('************************************************************************');

disp('------------------------------------------------------------------------');
disp('       Estimated Ergodic Distribution of State Variables');
est_psteady = zeros(nstate,1) ;
est_psteady(1) = sum(prod(+(aobs_1==repmat(vstate(1,:),size(aobs_1,1),1)),2))/nobs ;
est_psteady(2) = sum(prod(+(aobs_1==repmat(vstate(2,:),size(aobs_1,1),1)),2))/nobs ;
est_psteady(3) = sum(prod(+(aobs_1==repmat(vstate(3,:),size(aobs_1,1),1)),2))/nobs ;
est_psteady(4) = sum(prod(+(aobs_1==repmat(vstate(4,:),size(aobs_1,1),1)),2))/nobs ;

disp(est_psteady)

disp('       True Ergodic Distribution of State Variables');
disp(psteady)

disp('------------------------------------------------------------------------');
disp('       Estimated Conditional Choice Probabilities');
disp(freq_est_prob)
disp('       True Conditional Choice Probabilities');
disp(pequil)
disp('------------------------------------------------------------------------');




%// ----------------------------
%//  11. NPL Estimation
%// ----------------------------

disp('************************************************************************');
disp('************************************************************************');
disp('   ESTIMATION OF THE STRUCTURAL PARAMETERS OF THE DYNAMIC GAME');
disp('   BASED ON SIMULATED DATA FROM THE COMPUTED MPE');
disp('************************************************************************');
disp('************************************************************************');
disp('------------------------------------------------------------------------');
theta0 = zeros(kparam,1) ;

[best1,varb,like1]= npldygam(aobs,aobs_1,freq_est_prob,dfact,vstate,theta0,npliter,namesb) ;


disp('------------------------------------------------------------------------');
disp('       True value of paramaters:');
disp(['           Entry Cost / (sqrt(2) * ssigma)       =' num2str(ce/(sqrt(2)*ssigma))]) ;
disp(['           Monopoly Profit / (sqrt(2) * ssigma)  =' num2str(pi1/(sqrt(2)*ssigma))]) ;
disp(['            Duopoly Profit / (sqrt(2) * ssigma)  =' num2str(pi2/(sqrt(2)*ssigma))]) ;
disp('------------------------------------------------------------------------');
diary off ;



