function [llik,d1l,d2l]=logmlog(yd,x,Fx)


% Function returning likelihood, gradient and hessian of the logit model
% GAUSS Version - Victor Aguirregabiria - University of Toronto
% MATLAB translation - Claudio Lucinda - University of Sao Paulo


  myzero = 1E-16 ;
  nalt = size(Fx,2) ;
  kpar = size(x,2) ;

  llik = 0 ;
  j=1 ;
  while j<=nalt ;
    llik = llik + sum((yd==j).*log(Fx(:,j)+myzero)) ;
    j=j+1 ;
  end

  d1l = zeros(kpar*(nalt-1),1) ;
  d2l = zeros(kpar*(nalt-1),kpar*(nalt-1)) ;
  j=2 ;
  while j<=nalt ;
    indj1 = (j-2)*kpar + 1 ;
    indj2 = (j-1)*kpar ;
    d1l(indj1:indj2) = sum( x.*(repmat((yd==j) - Fx(:,j),1,size(x,2))));
    k=2 ;
    while k<=nalt
      indk1 = (k-2)*kpar + 1 ;
      indk2 = (k-1)*kpar ;
      if (j~=k) ;
        d2l(indj1:indj2,indk1:indk2) = (x.*repmat(Fx(:,j),1,size(x,2)))'*(x.*repmat(Fx(:,k),1,size(x,2))) ;
      end;
      if (j==k) ;
        d2l(indj1:indj2,indk1:indk2) = (x.*repmat(Fx(:,j),1,size(x,2)))'*(x.*repmat(Fx(:,k),1,size(x,2)))-(x.*repmat(Fx(:,j),1,size(x,2)))'*x ;
      end;
      k=k+1 ;
    end;
    j=j+1 ;
  end;
  
