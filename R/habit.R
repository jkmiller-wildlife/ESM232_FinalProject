#'
#'#'
#' @a coeff for initial funding & habitat area relationship
#' @b coeff for maintenance funding & habitat area relationship
#' @c coeff for habitat area change without action

# Let's keep this function to return a list or matrix or dataframe, instead of going into solver...
# I just wrote down random numbers and formula. Need to apply one that makes sense.
# To keep funding more...legible, let's say we use numerical unit of 1,000? (e.g. 100 equals to 100k dollars)

habitat_area = function(original_area=1, 
                        init_mgmt=T, maint_mgmt=T, 
                        init_funding=NA, maint_funding=NA,
                        n_time,
                        a=0.0002, b=0.001, c= -0.05){
  
  habitat_area = c(original_area)
  
  for (i in 2:n_time){
    
    # calculating restored area for each time step?
    if (init_mgmt == T & maint_mgmt == T){
      restored_area = (1 + a*init_funding/n_time + b*maint_funding + c) * habitat_area[i-1]
      } else if(init_mgmt == T & maint_mgmt == F){
        restored_area = (1 + a*init_funding/n_time + c) * habitat_area[i-1]
      } else if(init_mgmt == F & maint_mgmt == T){
        restored_area = (1 + b*maint_funding + c)* habitat_area[i-1]
      }else if (init_mgmt == F & maint_mgmt == F){
        restored_area = (1 + c) * habitat_area[i-1]
      } 
    
      habitat_area <- c(habitat_area, restored_area)
  }
  
  return(habitat_area)
}

x <-habitat_area(1, init_mgmt = T, maint_mgmt = T, init_funding = 400, maint_funding = 30, n_time = 30)
print(x)
