
function [newprob]=equilmap(prob0,argums)

% %// -----------------------------------------------------------------------
% %//  2. PROCEDURE FOR THE EQUILIBRIUM MAPPING 
% %// -----------------------------------------------------------------------
% %//
% %//  Format:     bestp = equilmap(prob0) 
% %//
% %//  Inputs:
% %//
% %//      prob0 = (nstate x 2) matrix with probabilities of being active
% %//              for the two players at every state
% %//              These probabilities represent firms' beliefs that will
% %//              be used to construct expected profits and value functions
% %//              and, given these expected values, we calculate best responses
% %//       argums = structure containing the following variables
% %//           -pi1, pi2,ce,vstate, ssigma,dfact
% %//
% %//  Outputs:
% %//
% %//      bestp = (nstate x 2) matrix with players' best response probabilities
% %//              given beliefs prob0.
% %//
% %// -----------------------------------------------------------------------

  ns = size(prob0,1) ;    %%// number of states
  newprob = prob0 ;
  
  fnames=fieldnames(argums);
for i=1:length(fnames)
    eval([fnames{i} '=argums.' fnames{i} ';']);
end

%   %// ------------------------------- 
%   %// a. Vectors of expected profits 
%   %// -------------------------------
  profit1_a0 = zeros(ns,1) ;        %%// Expected profit of firm 1 if own action=0
  profit1_a1 = (1-prob0(:,2)).*pi1+prob0(:,2).*pi2- ce.*(1-vstate(:,1)) ; %%// Expected profit of firm 1 if own action=1

%   %// Ex-ante expected profit of firm 1: before knowing the own epsilons
%   %//      Assumption about distribution of epsilon: normal distribution  
  eprofit1 = (1-prob0(:,1)).*profit1_a0+ prob0(:,1).*profit1_a1+ sqrt(2)*ssigma*normpdf(norminv(prob0(:,1),0,1),0,1) ;

  profit2_a0 = zeros(ns,1) ;        %// Expected profit of firm 2 if own action=0
  profit2_a1 = (1-prob0(:,1)).*pi1+prob0(:,1).*pi2- ce.*(1-vstate(:,2)) ; %// Expected profit of firm 2 if own action=1

  %// Ex-ante expected profit of firm 2: before knowing the own epsilons
  %//      Assumption about distribution of epsilon: normal distribution  
  eprofit2 = (1-prob0(:,2)).*profit2_a0+ prob0(:,2).*profit2_a1+ sqrt(2)*ssigma*normpdf(norminv(prob0(:,2),0,1),0,1) ;

  %// ----------------------------------------------------
  %//  b. Transition probabilities of the state variables
  %// ----------------------------------------------------
  %//
  %//  - (nstate x nstate) matrices of transition probabilities of 
  %//    state variables
  %//
  %//  - In these transition matrices:
  %//        - Rows represent current value of state variables
  %//        - Columns represent next period value of state variables
  %//            (the elements of each row should sum to 1)
  %//
  %//  - Remember: vstate = (0~0) | (0~1) | (1~0) | (1~1)
  %//
  %//  - Since the only state variables are the lagged values 
  %//    of firms' actions, all what we need to obtain the 
  %//    transition probabilities is the choice probabilities
  %//
  %//  - Transition matrices from the point of view of each firm:
  %//    Each firm knows his current action but not the current action
  %//    of his competitor
  %//
  %// ----------------------------------------------------

  %// Trasition matrix of firm 1 if current own action = 0
  %//  Remember: vstate = (0~0)     | (0~1)          | (1~0)       | (1~1)
  iptran1_a0 =[(1-prob0(:,2)) prob0(:,2)  zeros(ns,1) zeros(ns,1)];

  %// Trasition matrix of firm 1 if current own action = 1
  %//  Remember: vstate = (0~0)     | (0~1)          | (1~0)       | (1~1)
  iptran1_a1 =[zeros(ns,1) zeros(ns,1) (1-prob0(:,2)) prob0(:,2)];
    
  %// Trasition matrix of firm 2 if current own action = 0
  %//  Remember: vstate = (0~0)     | (0~1)          | (1~0)       | (1~1)
  iptran2_a0 =[(1-prob0(:,1)) zeros(ns,1) prob0(:,1) zeros(ns,1)];

  %// Trasition matrix of firm 2 if current own action = 1
  %//  Remember: vstate = (0~0)     | (0~1)          | (1~0)       | (1~1)
  iptran2_a1 =[zeros(ns,1) (1-prob0(:,1)) zeros(ns,1) prob0(:,1)];

  %// Ex-ante transition probability matrix
  %//    Not conditional on own action
  %//  Remember: vstate = (0~0) | (0~1) | (1~0) | (1~1)

   ftran =[(1-prob0(:,1)).*(1-prob0(:,2)) (1-prob0(:,1)).*prob0(:,2) ...
       prob0(:,1).*(1-prob0(:,2)) prob0(:,1).*prob0(:,2)];
  
  %//  -------------------------------------------------------
  %//  c. Calculating present values for each firm and state
  %//        Given that firms' believe that they will behave
  %//        in the future according to the probabilities prob0
  %//  -------------------------------------------------------
  %ftran = inv(eye(ns)-dfact*ftran) ;    %// 'Discounting' matrix
  value1 = (eye(ns)-dfact*ftran)\eprofit1 ;   %// Present values for firm 1
  value2 = (eye(ns)-dfact*ftran)\eprofit2 ;   %// Present values for firm 2
  
  %// Threshold value for firm 1:
  %//    Value of action 1 minus value of action 0 
  vtilda1 = (profit1_a1 + dfact * iptran1_a1 * value1) ...
      - (profit1_a0 + dfact * iptran1_a0 * value1) ;

  %// Threshold value for firm 2:
  %//    Value of action 1 minus value of action 0 
  vtilda2 = (profit2_a1 + dfact * iptran2_a1 * value2) ...
      - (profit2_a0 + dfact * iptran2_a0 * value2) ;

  %//  -------------------------------------------------------
  %//  d. Best response probabilities
  %//  -------------------------------------------------------
  newprob = [normcdf(vtilda1/(sqrt(2)*ssigma),0,1) normcdf(vtilda2/(sqrt(2)*ssigma),0,1)];

  