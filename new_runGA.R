library(GA)
#Monitor function 
monitor <- function(obj){
  iter <- obj@iter
  if (iter <= maxGenerations){
    fitness <- obj@fitness
    thisRunResults[iter,1] <<- max(fitness)
    thisRunResults[iter,2] <<- mean(fitness)    
    thisRunResults[iter,3] <<- median(fitness)
    cat(paste("\rGA | generation =", obj@iter, "Mean =", thisRunResults[iter,2], "| Best =", thisRunResults[iter,1], "\n"))
    flush.console()
  }
  else{                               #print error messages
    cat("ERROR: iter = ", iter, "exceeds maxGenerations = ", maxGenerations, ".\n")
    cat("Ensure maxGenerations == nrow(thisRunResults)")
  }
}
#RunGa function for runing the Problem code 
defaultControl = gaControl()
runGA2 = function(noRuns = 30){
  maxGenerations <<- 100 
  popSize = 300
  pcrossover = 0.8
  pmutation = 0.2
  type = "binary"
  #crv = 'gabin_spCrossover'
  selec = "gabin_nlrSelection"
  data <- getData()
  xx <- data[,-ncol(data)]
  yy <- data[,ncol(data)]
  fitness = svm_fitness  
  
  #Set up what stats you wish to note.    
  statnames = c("best", "mean", "median")
  thisRunResults <<- matrix(nrow=maxGenerations, ncol = length(statnames)) #stats of a single run
  resultsMatrix = matrix(1:maxGenerations, ncol = 1)  #stats of all the runs
  
  resultNames = character(length(statnames)*noRuns)
  resultNames[1] = "Generation"
  
  bestFitness <<- -Inf
  bestSolution <<- NULL
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
             names = colnames(xx), seed=i, popSize = popSize, 
             pcrossover = pcrossover, pmutation = pmutation,
             maxiter = maxGenerations,selection = selec,monitor= monitor)
    
    resultsMatrix = cbind(resultsMatrix, thisRunResults)
    if (GA@fitnessValue > bestFitness){
      bestFitness <<- GA@fitnessValue
      bestSolution <<- GA@solution
    }
    #Create column names for the resultsMatrix
    for (j in 1:length(statnames)) resultNames[1+(i-1)*length(statnames)+j] = paste(statnames[j],i)
  }
  colnames(resultsMatrix) = resultNames
  return (resultsMatrix)
  
}

getBestFitness<-function(){
  return(bestFitness)
}

getBestSolution<-function(){
  return(bestSolution)
}  
    