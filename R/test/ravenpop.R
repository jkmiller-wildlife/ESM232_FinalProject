#' @param t timesteps
#' @param P0 initial population
#' @param r intrinsic growth rate
#' @param K maximum pop (carrying capacity)
#' @param mgmt_action flag T or F
#' @param pred_mgmt_funding Funding, in unit of 1,000 USD
#' @param rmv_eff Removal efficiency per 1,000 USD?
#' @param min minimum population. We don't want to eradicate predator species?

raven_pop = function(t, P0, r, K, mgmt_action=F, pred_mgmt_funding, rmv_eff = 0.015, min=0.8){
  
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
        #P = min * P0 #Manage predator population not to go under certain number.
        #If we choose to just leave them alone for a while? 
        P = raven_pop[i] * exp(r)
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

x = raven_pop(20, 100, 0.1, 150, mgmt_action = T, pred_mgmt_funding = 6, rmv_eff= 0.015, min = 0.8)
y = raven_pop(20, 100, 0.1, 150, mgmt_action = T, pred_mgmt_funding = 15, rmv_eff= 0.015, min = 0.8)

print(x)
print(y)
