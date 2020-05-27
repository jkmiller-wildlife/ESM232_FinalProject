#' @param t timesteps
#' @param P0 initial population
#' @param r intrinsic growth rate
#' @param K maximum pop (carrying capacity)
#' @param mgmt_action flag T or F
#' @param pred_mgmt_funding Funding, in unit of 1,000 USD
#' @param rmv_eff Removal efficiency per 1,000 USD?
#' @param min minimum population. We don't want to eradicate predator species?


# Raven population mgmt.
raven_pop = function(t, P0, r, K, mgmt_action=F, pred_mgmt_funding, rmv_eff = 0.015, min=0.5){
  
  raven_pop = c(P0)
  
  for (i in 1:t){
    if (mgmt_action == T){
      pred_rmv = rmv_eff * pred_mgmt_funding # unit of thousands
      if (pred_rmv > 1){
        pred_rmv = 1 # Since you cannot remove predator that does not exists...
      }
      P = raven_pop[i] * (exp(r) - pred_rmv)
      if (P > K) {
        P = K
      } else if (P < min * P0){
        P = raven_pop[i] * exp(r)    #Just leave them alone for a while? because we don't care much about raven...
        #If we choose to manage raven population not to go under certain population.
        #P = min * P0 
      }
    } else if (mgmt_action == F){
      P = raven_pop[i] * exp(r)
      if (P > K) {
        P = K
      }
    }
    
    raven_pop = c(raven_pop, P)
  }
  
  
  return(c(raven_pop))
}



# faclong population mgmt
falcon_pop = function(t, P0, r, K, mgmt_action=F, pred_mgmt_funding, rmv_eff = 0.01, min=0.8){
  
  falcon_pop = c(P0)
  
  for (i in 1:t){
    if (mgmt_action == T){
      pred_rmv = rmv_eff * pred_mgmt_funding # unit of thousands
      if (pred_rmv > 1){
        pred_rmv = 1 # Since you cannot remove predator that does not exists...
      }
      P = falcon_pop[i] * (exp(r) - pred_rmv)
      if (P > K) {
        P = K
      } else if (P < min * P0){
        P = min * P0    # Manage falcon population not to go under certain number.
        #If we choose to just leave them alone for a while? 
        #P = falcon_pop[i] * exp(r)
      }
    } else if (mgmt_action == F){
      P = falcon_pop[i] * exp(r)
      if (P > K) {
        P = K
      }
    }
    
    falcon_pop = c(falcon_pop, P)
  }
  
  
  return(c(falcon_pop))
}

# Just for test purpose.
# x = raven_pop(20, 100, 0.1, 150, mgmt_action = T, pred_mgmt_funding = 8, rmv_eff= 0.015, min = 0.8)




#' @param raven vector of raven population(numeric vector?) over time
#' @param falcon vector of raven population(numeric vector?) over time
#' egg, chick, juveinle, and adult predation returns "NUMBER" of predation.
#' This means that survival rate that goes into Leslie Matrix should be calculated based on...
#' N of eggs, N of chicks, N of Juveniles, and N of adult(plover)

# Egg predation fuction. Returns a likely NUMBER of eggs to be predated. 
egg_predation = function(raven){
  pred_n = raven * 30
   
  return(pred_n)
}



chick_predation = function(raven, falcon){
  pred_n = raven *2 + falcon * 8
  
  return(pred_n)
}



juvenile_predation = function(falcon){
  pred_n = falcon * 6
  
  return(pred_n)
}



adult_predation = function(falcon){
  pred_n = falcon * 4
  
  return(pred_n)
}