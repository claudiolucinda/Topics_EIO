function discy = discthre(y,thre)

%** --------------------------------------------------------------
%** DISCTHRE - Discretization and Codification of a Variable using
%**            a prefixed vector of thresholds.
%**
%** by Victor Aguirregabiria
%**
%** --------------------------------------------------------------
%** Format       discy = discthre(y,thre)
%**
%** Input        y    - (nobs x 1) vector with observations of the
%**                     continuous variable
%**              thre - (k x 1) vector of thresholds
%**
%** Output       discy - (nobs x 1) vector with the codes of the 
%**                      discretized observations
%**                      Example: If y[i)>thre[5) and y[i)<=thre[6),
%**                      then discy[i)=6
%** --------------------------------------------------------------
%%**
%  Translation into MATLAB - Claudio R. Lucinda - University of Sao Paulo

  numcel = size(thre,1) ;
  discy = zeros(size(y,1),1) ;
  discy = discy + 1*( y<=thre(1) )  ;
  j=2 ;
  while j<=numcel ;
    discy = discy + j*(y>thre(j-1)).*(y<=thre(j)) ;
    j=j+1 ;
  end;
  discy = discy + (numcel+1)*( y>thre(numcel) )  ;

