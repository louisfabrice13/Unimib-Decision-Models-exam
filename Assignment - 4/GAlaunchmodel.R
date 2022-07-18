#general launcher for GenAlgo()

gacustom<-genalgo(popsize = , #number of individuals 
          epoche = , #number of iterations
          mutation=FALSE, #random gene-swap in offspring
          mutationRate=0, 
          diversify=FALSE, #keep track of biodiversity of gene-pool
          inbred=0)