function pxb=promlog(nalt,x,b)

% Computing multinomial Logit probabilities
% GAUSS Version - Victor Aguirregabiria - University of Toronto
% MATLAB translation - Claudio Lucinda - University of Sao Paulo

%  local xb, sumexpe, pxb ;
  b = [zeros(size(x,2),1) reshape(b,nalt-1,size(x,2))'];
  xb = x * b ;
  xb = xb - repmat(max(xb,[],2),1,size(xb,2)) ;
  sumexpe = sum(exp(xb')) ;
  pxb = exp(xb)./repmat(sumexpe',1,size(exp(xb),2)) ;


