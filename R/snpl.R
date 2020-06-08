#' Stage Structure Population Model using Leslie Matrix
#' 
#' @param fecundity fecundity rates
#' @param survivorship survivorship rates
#' @param Po initial population
#' @param nstep number of time steps
#' @param K carrying capacity
#' @return population structure for each time step (OR error message if population cannot be defined)


snpl_popK = function(fecundity, survivorship, Po, nstep, K) {
  
  nclasses = length(fecundity)
  
  # make sure inputs are in the right format
  if ((nclasses!=length(survivorship) ))
  { return("fecundity doesn’t match survivorship") }
  
  if ((nclasses!=length(Po) ))
  { return("initial population doesn’t match fecundity") }
  
  #initialize the Leslie matrix
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  leslie_matrix[1,] = fecundity
  
  for (i in 1:(nclasses-1)) {
    leslie_matrix[i+1,i] = survivorship[i]
  }
  leslie_matrix[nclasses,nclasses] = survivorship[nclasses]
  
  # create an matrix to store population structure
  pop_structure = matrix(nrow=nclasses, ncol=nstep)
  pop_structure[,1] = Po
  
  for (i in 2:nstep) {
    
    total_pop=sum(pop_structure[,i-1])
    
    # if we are using a carrying capacity adjust fertitlity if we are getting close
    if (K > 0) {
      #ratio = max(0, 1.0-total_pop/K) #Commented out by Donny Kim. Apply K only to adult population.
      ratio = max(0, 1.0-pop_structure[4,i-1]/K)
      #leslie_matrix[1,] = fecundity*ratio #Commented out by Donny Kim. Apply K only to adult population.
      leslie_matrix[1,4] = fecundity[4]*ratio
    }
    pop_structure[,i] = leslie_matrix %*% pop_structure[,i-1]
    
  }
  
  return(list(pop_structure=pop_structure, total_pop=total_pop))
}




############### First try to apply predator = Bad.
snpl_pop2 = function(fecundity, survivorship, Po, nstep, K, predation) {
  
  nclasses = length(fecundity)
  
  # make sure inputs are in the right format
  if ((nclasses!=length(survivorship) ))
  { return("fecundity doesn’t match survivorship") }
  
  if ((nclasses!=length(Po) ))
  { return("initial population doesn’t match fecundity") }
  
  #initialize the Leslie matrix
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  leslie_matrix[1,] = fecundity
  
  for (i in 1:(nclasses-1)) {
    leslie_matrix[i+1,i] = survivorship[i]
  }
  leslie_matrix[nclasses,nclasses] = survivorship[nclasses]
  
  # create an matrix to store population structure
  pop_structure = matrix(nrow=nclasses, ncol=nstep)
  pop_structure[,1] = Po
  
  for (i in 2:nstep) {
    
    total_pop=sum(pop_structure[,i-1])
    
    # if we are using a carrying capacity adjust fertitlity if we are getting close
    if (K > 0) {
      #ratio = max(0, 1.0-total_pop/K) #Commented out by Donny Kim
      ratio = max(0, 1.0-pop_structure[4,i-1]/K)
      #leslie_matrix[1,] = fecundity*ratio #Commented out by DOnny Kim
      leslie_matrix[1,4] = fecundity[4]*ratio
    }
    x = pop_structure[,i-1] - predation[,i]
    x[x<0] = 0
    x=matrix(x, ncol=1)
    #pop_structure[,i] = leslie_matrix %*% (pop_structure[,i-1] - predation[,i])
    pop_structure[,i] = leslie_matrix %*% (x)
    
  }
  
  return(list(pop_structure=pop_structure, total_pop=total_pop))
}





############### Final version that considers predator population and habitat area
snpl_pop3 = function(fecundity, survivorship, Po, nstep, K, predatorpop, area) {
  
  nclasses = length(fecundity)
  
  # make sure inputs are in the right format
  if ((nclasses!=length(survivorship) ))
  { return("fecundity doesn’t match survivorship") }
  
  if ((nclasses!=length(Po) ))
  { return("initial population doesn’t match fecundity") }
  
  #initialize the Leslie matrix
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  leslie_matrix[1,] = fecundity
  
  #for (i in 1:(nclasses-1)) {
  #  leslie_matrix[i+1,i] = survivorship[i]
  #}
  #leslie_matrix[nclasses,nclasses] = survivorship[nclasses]
  
  # create an matrix to store population structure
  pop_structure = matrix(nrow=nclasses, ncol=nstep)
  pop_structure[,1] = Po
  survivorship_t =c()
  
  for (i in 2:nstep) {
    
    leslie_matrix[1,] = fecundity * area[i]
    
    total_pop=sum(pop_structure[,i-1])
    
    
    
    ### Survivorship = 1-deathr
    ### deathr = 1 - survivorship
    
    ### deathr = predation + b = a * predator_pop + b
    ### deathr_0 = a * predator_0 + b
    
    ### let's assume 80% is gone by predation. and the other 20% is other cause.
    ### a = 0.8 * deathr_0/predator_pop0
    ### b = 0.2 * deathr_0 <- some kind of constant/intercept, but affected by snpl population change.
    ### b_t = 0.2 * deathr_0 * snpl_t/snpl_0
    
    ### deathr_t = a * predator_t + b_t
    ###          = 0.8 * deathr_0/predator_0 * predator_t + 0.2 * deathr_0 * snpl_t/snpl_0
    
    ### Survivorship_t = 1 - deathr_t
    ###                = 1- (0.8 * deathr_0/predator_0 * predator_t) - 0.2 * deathr_0 * total_t/total_0
    
    #egg
    survivorship_t[1] = 1 - 0.8 * (1-survivorship[1]) * predatorpop[1,i-1]/predatorpop[1,1] - 
      0.2 * (1-survivorship[1]) * pop_structure[1, i-1]/pop_structure[1, 1]
    
    #chick : raven 0.4 vs falcon 0.6 weight.
    survivorship_t[2] = 1 - 0.8 * (1-survivorship[2]) * (0.4* predatorpop[1,i-1]/predatorpop[1,1] + 0.6 *predatorpop[2,i-1]/predatorpop[2,1]) -
      0.2 * (1-survivorship[2]) * pop_structure[2, i-1]/pop_structure[2, 1]
    #Juvenile
    survivorship_t[3] = 1 - 0.8 * (1-survivorship[3]) * predatorpop[2,i-1]/predatorpop[2,1] - 
      0.2 * (1-survivorship[3]) * pop_structure[3, i-1]/pop_structure[3, 1]
    # Adult
    survivorship_t[4] = 1 - 0.8 * (1-survivorship[4]) * predatorpop[2,i-1]/predatorpop[2,1] -
      0.2 * (1-survivorship[4]) * pop_structure[4, i-1]/pop_structure[4, 1]
    
    survivorship_t[survivorship_t < 0] = 0
    
    ## put it into leslie matrix
    for (j in 1:(nclasses-1)) {
      leslie_matrix[j+1,j] = survivorship_t[j]
    }
    leslie_matrix[nclasses,nclasses] = survivorship_t[nclasses]
    
    
    
    
    # if we are using a carrying capacity adjust fertitlity if we are getting close
    if (K > 0) {
      #ratio = max(0, 1.0-total_pop/K) #Commented out by Donny Kim
      ratio = max(0, 1.0-pop_structure[4,i-1]/K)
      #leslie_matrix[1,] = fecundity*ratio #Commented out by DOnny Kim
      leslie_matrix[1,4] = fecundity[4]*ratio
    }
    #x = pop_structure[,i-1] - predation[,i]
    #x[x<0] = 0
    #x=matrix(x, ncol=1)
    pop_structure[,i] = as.integer(leslie_matrix %*% (pop_structure[,i-1]))
    #pop_structure[,i] = leslie_matrix %*% (x)
    
  }
  
  return(list(pop_structure=pop_structure, total_pop=total_pop))
}