# B. Definition of the cost function

myopic_costs=function(S, MF, params, p){
  
  "This function computes the myopic expected cost associated with each decision for each state, 
   and returns an array of state/decision costs.
    
   Takes:
    * An integer S, describing the possible states of the bus
    * A maintenance cost function MF, which takes a vector of parameters and a 'state' argument
    * A vector params, to be supplied to the maintenance cost function MF. The first element of 
      the vector is the replacement cost rc.
    * A (3x1) vector p describing the state transitions probabilities 
        
    Returns:
    * A (Sx2) array containing the maintenance and replacement costs for the N possible states of the bus"
  
  rc = params[1]
  maint_cost = rep(NA,S)
  repl_cost = rep(NA,S)
  
  for(s in 1:S){
    maint_cost[s] = MF(s,params[-1])
    repl_cost[s] = rc
  }
  
  cbind(maint_cost,repl_cost)
}

# C. Definition of the choice probabilities, as a function of an array of costs

choice_prob=function(cost_array){
  
  # Returns the probability of each choice, conditional on an array of state/decision costs.
  
  S = nrow(cost_array)
  cost = cost_array-apply(cost_array,1,min) #take the difference since 1) results are the same 2) more stable with exp()
  util = exp(-cost)
  pchoice = util/rowSums(util)
  
  pchoice
}
