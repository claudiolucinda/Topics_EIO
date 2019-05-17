function fmat = tranprob(xobs0,inda,id0,xval,fixdel,vdel0,vdel1,vomega)


% /*
% ** ---------------------------------------------------------------------
% ** TRANPROB.SRC 
% ** It estimates Markov transition probabilities f(x(t+1) | x(t), a(t))
% ** where x is a vector of K variables, a(t) is discrete, and the
% ** form of the transition rule for variable xk is:
% **
% **  xk(t+1) = delta0(k,a(t)) + delta1(k,a(t)) * xk(t) + omega_k(t)
% **
% ** where {delta(k,1),...,delta(k,J)} are parameters, and
% ** omega_k(t) is an iid random shock independent of x(t) and with 
% ** unknown distribution (i.e., the distribution this shock is estimated 
% ** non-parametrically)
% **               
% ** by Victor Aguirregabiria
% **
% ** ---------------------------------------------------------------------
% **
% **  Format of the procedure:
% **
% **  fmat = tranprob(indx,inda,id,xval,fixdel,vdel0,vdel1,vomega)
% **
% **  INPUTS:
% **          xobs    - (nobs x kvar) vector with observed values of x
% **
% **          inda    - (nobs x 1) vector with observed indexes of a
% **
% **          id      - (nobs x 1) vector with the ID number of 
% **                    individuals (for panel data). 
% **                    Note: if the data is not a panel data set,
% **                    this variable should be set to zero
% **
% **          xval    - (numx x kvar) vector with the discretized 
% **                    support of x
% **
% **          fixdel  - (K x J) matrix of zeros and ones
% **                    fixdel(k,j)=0 ==> delta(k,j) to be estimated
% **                    fixdel(k,j)=1 ==> delta(k,j) is fixed by user
% **
% **          vdel0   - (K x J) matrix where entry "vdel0(k,j)" is 
% **                    the fixed value of "delta0(k,j)" provided
% **                    by the user. If the parameter "delta0(k,j)" is
% **                    to be estimated from the data, "vdel0(k,j)" 
% **                    can be arbitrary
% **
% **          vdel1   - (K x J) matrix where entry "vdel1(k,j)" is 
% **                    the fixed value of "delta1(k,j)" provided
% **                    by the user. If the parameter "delta1(k,j)" is
% **                    to be estimated from the data, "vdel1(k,j)" 
% **                    can be arbitrary
% **
% **          vomega  - (K x J) matrix of zeros and ones
% **                    "vomega(k,j)=0" ==> Deterministic transition
% **                    "vomega(k,j)=1" ==> Stochastic transition
% **
% **  OUTPUTS:
% **          fmat    - (numx x numx*nchoice) matrix with the conditional
% **                    choice transition probabilities of the vector x
% **                    fmat1 ~ fmat2 ~ ... ~ fmatJ
% **
% ** ---------------------------------------------------------------------
% */ Translation into MATLAB - Claudio R. Lucinda - University of Sao Paulo
  
  nobs = size(xobs0,1) ;
  kvar = size(xobs0,2) ;
  numx = size(xval,1) ;
  nchoice = max(inda) ;
  
  disp('     ---------------------------------------------------------');
  disp('     ESTIMATION OF CONDITIONAL CHOICE TRANSITION PROBABILITIES');
  disp('     ---------------------------------------------------------');
  
%   @ ----------------------------------------------------- @
%   @ Creating x(t+1) and removing last obs. of each indiv. @
%   @ ----------------------------------------------------- @
  xobs1 = [xobs0(2:nobs,:);zeros(1,kvar)];
  if id0==0;
    xobs0 = xobs0(1:nobs-1,:) ;
    xobs1 = xobs1(1:nobs-1,:) ;
    inda = inda(1:nobs-1) ;
  else
    id1 = [id0(2:nobs);0];
    xobs0=xobs0(id0==id1,:);
    xobs1=xobs1(id0==id1,:);
    inda=inda(id0==id1,:);
  end;
    
%   @ --------------------------------------------------------- @
%   @ Estimating parameters "delta" and distribution of "omega" @
%   @ --------------------------------------------------------- @
  ncelome = 101 ;
  cdfome = zeros(ncelome,kvar) ;
  omeval = zeros(ncelome,kvar) ;
  disp('---------------------------------------------------------');
  k=1 ;
  while k<=kvar ;
    disp(['State variable: ' num2str(k)]);
  disp('---------------------------------------------------------');
  j=1 ;
  while j<=nchoice ;
      if fixdel(k,j)==0
        selxkj0=xobs0(inda==j,k);
        selxkj1=xobs1(inda==j,k);
        buff = [ones(size(selxkj1,1),1) selxkj0];
        sdbuff = (buff'*buff)\(buff'*selxkj1) ;
        vdel0(k,j) = sdbuff(1) ;
        vdel1(k,j) = sdbuff(2) ;
        sdbuff = sum((selxkj1-buff*sdbuff).*(selxkj1-buff*sdbuff)) ;
        sdbuff = sdbuff/size(selxkj1,1) ;
        sdbuff = sqrt(diag(sdbuff*inv(buff'*buff))) ;
      end
      if fixdel(k,j)==1;
        sdbuff = [0;0];
      end;
      disp(['delta0( ' num2str(j) ' ) = ' num2str(vdel0(k,j))]);
      disp(['S.E. delta0(' num2str(j) ') =' num2str(sdbuff(1))]);
      disp(['delta1(' num2str(j) ') =' num2str(vdel1(k,j))]);
      disp(['S.E. delta1(' num2str(j) ') =' num2str(sdbuff(2))]);
      j=j+1 ;
  end ;
    disp('---------------------------------------------------------');
    omekobs = xobs1(:,k)- (vdel0(k,inda)')- (vdel1(k,inda)').*xobs0(:,k) ;
    minome = pctiles(omekobs,1) ;
    maxome = pctiles(omekobs,99) ;
    stepome = floor(1e6*(maxome-minome)/(ncelome-1))/1e6 ;
    omeval(:,k) = (minome:stepome:minome+(ncelome-1)*stepome)' ;
    cdfome(:,k) = kernel1(omekobs,omeval(:,k)) ;
    
    scatter(omeval(:,k),cdfome(:,k)) ;
    title('Estimated PDF of Monthly Mileage (omega)') ;
    cdfome(:,k) = cdfome(:,k)./sum(cdfome(:,k)) ;
    cdfome(:,k) = cumsum(cdfome(:,k)) ;    
    k=k+1 ;
  end ;
  clear xobs0 xobs1 inda omekobs selxkj0 selxkj1 ;
    
%   @ ------------------------------------------ @
%   @ Construction of transition probs. matrices @
%   @ ------------------------------------------ @
  fmat = zeros(numx,1) ;
  j=1 ;
  while j<=nchoice ;
    fmatj = 1 ;
    k=1 ;
    while k<=kvar ;          
      xkval = sort(xval(:,k),1) ;
      xkval=xkval(xkval~=[(xkval(1)-1);xkval(1:size(xkval,1)-1)]) ;
      valomekj = repmat(xkval',size(xkval,1),1) - vdel0(k,j) - vdel1(k,j).*repmat(xkval,1,size(xkval,1)) ;
      fmatjk = cdfome(1,k).*(valomekj<=omeval(1,k)) ;
      iome=2 ;
      while iome<=(size(omeval,1)-1) ;
        fmatjk = fmatjk+(cdfome(iome,k)-cdfome(iome-1,k)).*(valomekj>omeval(iome-1,k)).*(valomekj<=omeval(iome,k));
        iome=iome+1 ;
      end;
      fmatjk = fmatjk +(1-cdfome(iome-1,k)).*(valomekj>omeval(iome,k)) ;
      fmatjk = fmatjk./repmat(sum(fmatjk,2),1,size(fmatjk,2));
      fmatj = kron(fmatj,fmatjk);
      k=k+1 ;
    end;
    fmat=[fmat fmatj];
    j=j+1 ;
  end ;   
  fmat = fmat(:,2:size(fmat,2)) ;
