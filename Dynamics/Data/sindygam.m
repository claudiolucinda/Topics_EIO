function [simul_a,simul_a_1]=sindygam(numobs,pchoice,pste,vecs)
%// -----------------------------------------------------------------------
%//  5. PROCEDURE TO SIMULATE DATA FROM THE COMPUTED EQUILIBRIUM
%// ---------------------------------------------------------------------
  nums = size(vecs,1) ;
  nump = size(pchoice,2) ;
  pbuff1 = cumsum(pste) ;
  pbuff0 = cumsum([0;pste(1:nums-1)]) ;
  uobs = rand(numobs,1) ;
  uobs = (repmat(uobs,1,size(pbuff0,1))>=repmat(pbuff0',size(uobs,1),1)).*(repmat(uobs,1,size(pbuff1,1))<=repmat(pbuff1',size(uobs,1),1)) * (1:1:nums)' ;
  simul_a_1 = vecs(uobs,:) ;
  simul_a = (rand(numobs,nump)<=pchoice(uobs,:)) ;
  