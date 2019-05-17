function [equil_q,equil_p,equil_profit]=cournot_equil(ttt,argums)

% @ ------------------------------------------------------------------------- @
% @   PROCEDURE:                                                              @
% @       Format:                                                             @
% @          { equil_q, equil_p, equil_profit } = cournot_equil(ttt) ;        @
% @                                                                           @
% @       Inputs:                                                             @
% @           ttt = Scalar value between 1 and numt. It represents the index of a year @
% @           argums=structures with components
% @             mc,ad_q,maxnfirm,ademand,ead_q,fixedc
% @       Output:                                                             @
% @           equil_q = (numm x maxnfirm) matrix with equilibrium output      @
% @                      at period ttt for every market at (rows) and for     @
% @                      every possible value of the number of firms in the   @
% @                      market between 1 and maxnfirm (columns)              @
% @                                                                           @
% @           equil_p = (numm x maxnfirm) matrix with equilibrium price       @
% @                      at period ttt for every market at (rows) and for     @
% @                      every possible value of the number of firms in the   @
% @                      market between 1 and maxnfirm (columns)              @
% @                                                                           @
% @           equil_profit = (numm x maxnfirm) matrix with equilibrium profit @
% @                      at period ttt for every market at (rows) and for     @
% @                      every possible value of the number of firms in the   @
% @                      market between 1 and maxnfirm (columns)              @
% @
% @                                                                           @
% @ ------------------------------------------------------------------------- @
% Translation into MATLAB - Claudio Lucinda - University of Sao Paulo

fnames=fieldnames(argums);
for i=1:length(fnames)
    eval([fnames{i} '=argums.' fnames{i} ';']);
end

  matnumn = ones(numm,1)*(1:maxnfirm);

%  // Matrix with equilibrium price
  equil_p = repmat(mc(:,ttt),1,maxnfirm)./(1-((ad_q+repmat(ead_q(:,ttt),1,maxnfirm))./matnumn)) ;

%  // Matrix with equilibrium output
  equil_q = (repmat(ademand(:,ttt),1,maxnfirm)-log(equil_p))./(ad_q+repmat(ead_q(:,ttt),1,maxnfirm)) ;
  equil_q = exp(equil_q) ;

% // Matrix with equilibrium profir
  equil_profit = (equil_p-repmat(mc(:,ttt),1,maxnfirm)).*(equil_q./matnumn) - repmat(fixedc(:,ttt),1,maxnfirm) ;

