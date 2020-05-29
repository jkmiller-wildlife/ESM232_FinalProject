#' Population Evolution using Leslie Matrix
#' Evolve a population
#' #######Copied from class files. Modify as needed. ########
#' @param fecundity fecundity rates
#' @param survivorship survivorship rates
#' @param Po initial population
#' @param nstep number of time steps
#' @param K carrying capacity
#' @return population structure for each time step (OR error message if population cannot be defined)


evolve_popK = function(fecundity, survivorship, Po, nstep,K=0) {
  
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
  pop.structure = matrix(nrow=nclasses, ncol=nstep)
  
  pop.structure[,1] = Po
  
  for (i in 2:nstep) {
    
    total.pop=sum(pop.structure[,i-1])
    
    # if we are using a carrying capacity adjust fertitlity if we are getting close
    if (K > 0) {
      ratio = max(0, 1.0-total.pop/K)
      leslie_matrix[1,] = fecundity*ratio
    }
    pop.structure[,i] = leslie_matrix %*% pop.structure[,i-1]
    
  }
  
  return(pop.structure)
}
