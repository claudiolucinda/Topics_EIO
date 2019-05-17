function [xval,indx0]=disckpie(xobs,dtype,minpct,maxpct,numx)

%**
%** -----------------------------------------------------------------------
%** DISCKPIE.SRC - It discretizes a vector of variables according
%**                to a criterion selected by the user
%**
%** by Victor Aguirregabiria
%**
%** -----------------------------------------------------------------------
%**  Format of the procedure:
%**
%** { xval, indx0 } = disckpie(xobs,dtype,minpct,maxpct,numx)
%**
%**  INPUTS:
%**          xobs    - (nobs x kvar) matrix with observations 
%**                    of variables
%**
%**          dtype   - (kvar x 1) with discretization criteria
%**                  "dtype(j)=1": Variable "j" is discrete and 
%**                                not more discretization is wanted
%**                  "dtype(j)=2": Variable "j" will be discretized   
%**                                using a uniform grid in the space
%**                                of this variable
%**                  "dtypea(j)=3": Variable "j" will be discretized
%**                                 using a uniform grid in the space
%**                                 of the probability distribution 
%**                                 of this variable
%**
%**          minpct  - (kvar x 1) vector with percentiles for the 
%**                    minimum values in the discretized spaces
%**
%**          maxpct  - (kvar x 1) vector with percentiles for the 
%**                    maximum values in the discretized spaces
%**
%**          numx    - (kvar x 1) vector with number of cells in 
%***                   the discretized spaces
%**
%**  OUTPUTS:
%**          xval    - (totnumx x kvar) matrix with the discretized 
%**                    support of the variables. Column "j" corresponds
%**                    to variable "j". Rows are sorted by variables,
%**                    first by variable 1, second by variable 2, and so on
%**
%**          indx0   - (nobs x 1)  vector with indexes of the discretized 
%**                    observations of x. That is, if the discretized value
%**                    of xobs(i) is xval(j), then indx0(i)=j
%** -----------------------------------------------------------------------
% Translation into MATLAB - Claudio R. Lucinda - University of Sao Paulo
        
  if (sum((minpct<0)+(minpct>100))>0) ;
    disp('Error: "minpctx" should be between 0 and 100') ;
    return;
  elseif (sum((maxpct<0)+(maxpct>100))>0) ;
    disp('Error: "maxpctx" should be between 0 and 100');
    return;
  elseif (sum(maxpct<=minpct)>0) ;
    disp('Error: "maxpctx" should be greater than "minpctx"');
    return;
  end
  
  if (size(xobs,2)~=size(minpct,1)) ;
    disp('Error: # variables in xobs is different to rows in "minpctx"');
    return;
  elseif (size(xobs,2)~=size(maxpct,1)) ;
    disp('Error: # variables in xobs is different to rows in "maxpctx"');
    return;
   elseif (size(xobs,2)~=size(maxpct,1)) ;
    disp('Error: # variables in xobs is different to rows in "maxpctx"');
    return;
  end;
  
  nobs = size(xobs,1) ;
  totnumx = prod(numx) ;
  kvar = size(xobs,2) ;
  xval  = zeros(totnumx,kvar) ;
  indx0 = zeros(nobs,1) ;
  
  if (kvar==1) ;
    if (dtype==1) ;
      buff = sort(xobs,1) ;
      buff_1 = [-999999 ;buff(1:nobs-1)];
      xval=buff(buff~=buff_1) ;
      stepx = 0.0001 ;
      xthre = xval(1:numx-1) + stepx ;
      indx0 = discthre(xobs,xthre) ;
    elseif (dtype==2) ;
      minx = pctiles(xobs,minpct) ;
      maxx = pctiles(xobs,maxpct) ;
      stepx = floor( 1e6 * (maxx-minx)/(numx-1) ) / 1e6 ;
      xthre = (minx+stepx:stepx:(minx+(numx-2)*stepx))';
      xval = (minx:stepx:minx+(numx-1)*stepx)';
      indx0 = discthre(xobs,xthre) ;
    elseif (dtype==3) ;
      stepx = int( 1000 * (maxpct-minpct)/(numx-1) ) / 1000 ;
      xthre = minpct+stepx:stepx:minpct+stepx+(numx-2)*stepx;
      xthre = pctiles(xobs,xthre);
      xval = (minpct:stepx:minpct+(numx-1)*stepx)';
      xval = pctiles(xobs,xval) ;
      indx0 = discthre(xobs,xthre) ;
    end;    
  elseif (kvar>1) ;
    j=1 ;
    while j<=kvar ;
      if dtype(j)==1
        buff = sort(xobs(:,j),1) ;
        buff_1 = [-999999;buff(1:nobs-1)];
        xv=buff(buff~=buff_1) ;
        stepx = 0.0001 ;
        xthre = xv(1:numx(j)-1) + stepx ;
        i0 = discthre(xobs(:,j),xthre) ;      
      elseif dtype(j)==2;
        minx = pctiles(xobs(:,j),minpct(j)) ;
        maxx = pctiles(xobs(:,j),maxpct(j)) ;
        stepx = int( 1e6 * (maxx-minx)/(numx(j)-1) ) / 1e6 ;
        xthre = minx+stepx:stepx:(minx+(numx(j)-2)*stepx);
        xv = minx:stepx:(minx+(numx(j)-1)*stepx);
        i0 = discthre(xobs(:,j),xthre) ;
      elseif dtype(j)==3;
        stepx = int( 1000 * (maxpct(j)-minpct(j))/(numx(j)-1))/1000 ;
        xthre = minpct(j)+(stepx:stepx:numx(j)-1);
        xthre = pctiles(xobs(:,j),xthre);
        xv = minpct(j):stepx:(minpct(j)+(numx(j)-1)*stepx);
        xv = pctiles(xobs(:,j),xv);
        i0 = discthre(xobs(:,j),xthre) ;
      end    
      if j==1
        xval(:,j) = kron(xv,ones(prodc(numx(j+1:kvar)),1));
        indx0 = indx0 + (i0-1)*prodc(numx(j+1:kvar)) ;        
      elseif j>1 && j<kvar
        xval(:,j) = kron(ones(prodc(numx(1:j-1)),1),kron(xv,ones(prodc(numx(j+1:kvar)),1)));
        indx0 = indx0 + (i0-1)*prodc(numx(j+1:kvar)) ;                
      elseif (j==kvar) ;
        xval(:,j) = kron(ones(prodc(numx(1:j-1)),1),xv);
        indx0 = indx0 + i0 ;
      end
      j=j+1 ;
    end
  end
  
