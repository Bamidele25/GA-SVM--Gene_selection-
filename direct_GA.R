#RunGa function for runing the Problem code 
defaultControl = gaControl()
  maxGenerations <<- 50 
  popSize = 250
  pcrossover = 0.8
  pmutation = 0.2
  type = "binary"
  #crv = 'gabin_spCrossover'
  selec = "gabin_nlrSelection"
  data <- getData()
  xx <- data[,-ncol(data)]
  yy <- data[,ncol(data)]
  fitness = svm_fitness  
  
  GA <- ga(type=type, fitness = fitness, xx=xx, yy=yy, nBits = ncol(xx), 
           names = colnames(xx),popSize = popSize, 
           pcrossover = pcrossover, pmutation = pmutation,
           maxiter = maxGenerations,selection = selec,monitor= monitor)
  
plot(GA)
  