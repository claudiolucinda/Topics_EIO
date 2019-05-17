function pc= pctiles(y,vecprob)

%** ---------------------------------------------------------------
%** PCTILES - Obtain empirical pencentiles of a variable
%**
%** by Victor Aguirregabiria
%**
%** ---------------------------------------------------------------
%** Format:     pcy = pctiles(y,p)
%**
%** Input:      y    - (nobs x 1) vector of observations
%**             p    - (k x 1) vector of probabilities in %
%**                    It should be sorted from the minimum to
%**                    the maximum percentile
%**                    Example: p = ( 5 | 15 | 75 | 99 )
%**
%** Output:     pcy  - (k x1) vector of percentiles
%** ---------------------------------------------------------------
%**
% Translation into MATLAB - Claudio Lucinda University of Sao Paulo
%  local num, sorty, indexpc, pc ;
  if sum((vecprob<0)+(vecprob>100))>0 ;
    disp('Error: Probabilities should be between 0 and 100');
    return;
  end;
  num = size(y,1) ;
  vecprob = vecprob/100 ;
  y = sort(y,1) ;
  indexpc = round(vecprob*num) ;
  indexpc = indexpc.*(indexpc>0) + 1*(indexpc==0) ;
  pc = y(indexpc) ;
  if vecprob(1)==0 ;
    pc(1) = min(y) ;
  end ;
  if vecprob(size(vecprob,1))==1 ;
    pc(size(vecprob,1)) = max(y) ;
  end;
  

