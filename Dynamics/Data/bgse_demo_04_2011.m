
% %// ******************************************************************************
% %//
% %//  bgse_demo_04_2011.prg
% %//                                                                 
% %//  THIS PROGRAM ESTIMATES DYNAMIC DISCRETE CHOICE STRUCTURAL MODEL
% %//  WITHIN THE CLASS OF MODELS PROPOSED IN RUST (Handbook of Econometris, 1994)
% %//
% %//  THE ESTIMATION ALGORITHM IS NESTED PSEUDO LIKELIHOOD (NPL) AS IN 
% %//  AGUIRREGABIRIA & MIRA (Econometrica, 2002). 
% %//
% %//  FOR SINGLE-AGENT DP MODELS, THE NPL ALGORITHM GENERATES A GENERAL CLASS
% %//  OF ESTIMATORS THAT CONTAINS AS SPECIFIC CASES:
% %//      - HOTZ-MILLER CCP ESTIMATOR (when #iterations = 1)
% %//      - K-step estimators
% %//      - MAXIMUM LIKELIHOOD ESTIMATOR (when algorithm iterates until convergence)
% %//
% %//  HERE, AS AN EXAMPLE, I APPLY THIS COMPUTER CODE TO THE MACHINE REPLACEMENT 
% %//  MODEL and THE BUS ENGINE DATASET IN RUST (Econometrica, 1987) 
% %//
% %//  by Victor Aguirregabiria
% %//
% %//  Barcelona, July 2011
% %//
% %// ******************************************************************************
% 
% 
% %//  ---------------------------------------------------------------------
% %//                              MODEL
% %//  ---------------------------------------------------------------------
% %//      (1) Variables and parameters:
% %//      -----------------------------
% %//          a      = Q decision variables: a = {a1, a2,... aQ}
% %//                   If they are not discrete, they are discretized
% %//                   according to a criterion provided by the user
% %//
% %//          indexa = Index of the discrete choice:
% %//                   "indexa" belongs to {1,2,...,J}
% %//
% %//          x      = K observable state variables: x = {x1, x2,... xK}
% %//                   If they are not discrete, they are discretized
% %//                   according to a criterion provided by the user
% %//
% %//          eps    = (eps[1),eps[2),...,eps[J)) = Unobservable state vars
% %//
% %//          alpha  = Vector of structural parameters in preferences
% %//
% %//          beta   = Discount factor
% %//
% %//      (2) One-period utility functions: 
% %//      --------------------------------
% %//          For a=1,2,...,J:    U(a,x,e) = u(a,x) + eps[a)
% %//
% %//          where:  u(a,x) = z(a,x) * alpha
% %//
% %//      Example:    u(1,x1,x2) = c*w(1,x1,x2)
% %//                  u(2,x1,x2) = b1 + c*w(2,x1,x2) + b2*x3
% %//                  u(3,x1,x2) = b3 + c*w(3,x1,x2) + b4*x3
% %//
% %//              alpha = (c|b1|b2|b3|b4) ;
% %//              z(1,x) = ( w(1,x1,x2) ~ 0 ~ 0  ~ 0 ~ 0  )
% %//              z(2,x) = ( w(2,x1,x2) ~ 1 ~ x3 ~ 0 ~ 0  )
% %//              z(3,x) = ( w(3,x1,x2) ~ 0 ~ 0  ~ 1 ~ x3 )
% %//               
% %//      (3) Conditional transition probabilities of x: f(x'|x,a)
% %//      --------------------------------------------------------
% %//      The transition rule of state variable xk is:
% %//
% %//      xk[t+1) = delta0(k,a[t)) + delta1(k,a[t)) * xk[t) + omega_k[t)
% %//
% %//      where {delta(k,1),...,delta(k,J)} are parameters, and
% %//      omega_k[t) is an iid random shock, independent of x[t) 
% %//      and with unknown distribution 
% %//      (i.e., the distribution is estimated non-parametrically)
% %//
% %//      (4) Distribution of unobservables:
% %//      ----------------------------------
% %//           (eps[1),eps[2),...,eps[J)) are extreme value distributed
% %//
% %//  ----------------------------------------------------------------------------
% Translated into MATLAB by Claudio Lucinda - University of Sao Paulo


clear; clc;

%// ************************
%//  1. Some Constants
%// ************************

jchoice = 2 ; %// Number of values of the discrete decision variable
kvarx = 1 ;   %// Number of state variables

vcelx = 400 ; %// Number of cells in discretization of state variable
dtypex = 2 ;  %// Code for the type of discretization we will make for the
              %// state variable:
              %//    dtypex = 1  means state variable is already discrete
              %//                and no more discretization is needed
              %//    dtypex = 2  means state variable will be discretized
              %//                using a grid that is uniform in the space
              %//                of the variable
              %//    dtypex = 3  means state variables will be discretized
              %//                using a grid that is uniform in the space
              %//                of percentiles of the empirical distribution
              %//                of the variable

minpctx = 1 ; %// Minimum value in the discretization of state variables.
              %// It is a percentile in the empirical distribution of x
              %// and therefore it should be a number between 0 and 100

maxpctx = 99; %// Maximum valus in the discretization of state variables.
              %// It is a percentile in the empirical distribution of x
              %// and therefore it should be a number between 0 and 100


%// **********************************
%//  2. Data file and and output file
%// ***********************************


%// Name and address of output file
%fileout = "c:\\documents\\COURSES\\Barcelona_GS_summer_school\\practice\\bgse_demo_04_2011.out" ;

% Change dir here

wdir='C:\Documents and Settings\Claudio Lucinda\Meus documentos\Pesquisa\Curso Verão 2011\Summer course Barcelona\Datasets and code\';
path(wdir,path);
data=dlmread([wdir 'bus1234.txt'],'\t',1,0);

diary([wdir 'bsge_demo_04_2011.out']);

%// Name and address of data file
%filedat = "c:\\documents\\COURSES\\Barcelona_GS_summer_school\\practice\\bus1234.dat" ;

nobs = 8260 ;   %// Total number of observations
nindiv = 104 ;  %// Total number of individuals

%// ******************************************
%//  3. Reading data and some transformations - mexer nisso aqui depois
%// ******************************************
%output file = ^fileout reset ;
%open dtin = ^filedat for read varindxi ;
%data = readr(dtin,nobs);

iobs = data(:,1) ;     %// Bus code variable
aobs = data(:,5) ;      %// Engine replacement dummy
xobs = data(:,6) ;  %// Cumulative miles
xobs = xobs/1000 ;          %// x is measure in thousands of miles






%// **************************************************************
%//   5. Calling procedure for discretization of state variables
%// **************************************************************
[xval,indobsx] = disckpie(xobs,dtypex,minpctx,maxpctx,vcelx) ;
ncelx = size(xval,1) ;

dtypea = 1;
minpcta = 0 ;
maxpcta = 100 ;
vcela = 2 ;
[aval,indobsa] = disckpie(aobs,dtypea,minpcta,maxpcta,vcela) ;
ncela = size(aval,1) ;

%// ***********************************************************
%//  6. Specification of utility function and discount factor
%// ***********************************************************

%// Vector with names of the parameters
namespar = {'repcost';'mcost1';'mcost2'};


% transition matrices - discussed on the morning class.
% zmat1 = matriz de transição da opção de NÃO SUBSTITUIR
% zmat2 = matriz de transição da opção de SUBSTITUIR
%// Note: The columns of the matrices "zmatj" below
%//  should be consistent with the order of the parameters
%//  in 'namespar' above, i.e., 
%//      first column of "zmatj" corresponds to first parameter
%//      second column of "zmatj" corresponds to second parameter
%//      and so on
zmat1 = [zeros(ncelx,1) (-xval(:,1)) (-xval(:,1).*xval(:,1))] ;
zmat2 = [(-ones(ncelx,1)) zeros(ncelx,2)];
zmat = [zmat1 zmat2];
clear zmat1 zmat2 ;

%// Value of time discount factor
% Any DF between 0.8 and 1 are observacionalmente equivalentes. Ou seja
% tanto faz qual valor escolher.

beta = 0.99 ;   


%// ************************************************
%//  8. Transition probabilities of state variables 
%// ************************************************

fixdel = [0 1];
vdel0 = [0 0];
vdel1 = [1 0];
vomega = [1 1];
fmat = tranprob(xobs,indobsa,iobs,xval,fixdel,vdel0,vdel1,vomega) ;

%// *************************************************************************
%//   10. Reduced form estimation of Conditional Choice Probs 
%// *************************************************************************

%// We use a logit model where the explanatory variables consist of
%//  polynomial series of the state variables

xprob0 = [ones(ncelx,1) xval(:,1) (xval(:,1).*xval(:,1)) (xval(:,1).*xval(:,1).*xval(:,1))];
[best,varest]= multilog(indobsa,xprob0(indobsx,:)) ;
best = reshape(best,jchoice-1,size(xprob0,2))' ;
prob0 = [zeros(ncelx,1) (xprob0 * best)];
prob0 = exp(prob0-max(prob0,2)) ;
prob0 = prob0./repmat(sum(prob0,2),1,size(prob0,2)) ;

scatter(xval,prob0(:,2));
title('Estimated CCP for Replacement') ;
pause(10) ;

% Gerando nros aleatórios de uma distribuição aleatória pra checar se tá OK
prob0 = rand(ncelx,1) ;
prob0 = [prob0 (1-prob0)];


%// ***************************
%//  12. Sructural estimation 
%// ***************************
kstage = 20 ;    %// Maximum number of NPL iterations

%// Calling procedure for NPL estimator

[tetaest , ~, pest]=kpie(indobsa,indobsx,zmat,prob0,beta,fmat,kstage,namespar);

diary off