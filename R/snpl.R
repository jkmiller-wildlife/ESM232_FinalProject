#' Age Structure Population Model using Leslie Matrix
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
      ratio = max(0, 1.0-total_pop/K)
      leslie_matrix[1,] = fecundity*ratio
    }
    pop_structure[,i] = leslie_matrix %*% pop_structure[,i-1]
    
  }
  
  return(list(pop_structure=pop_structure, total_pop=total_pop))
}
