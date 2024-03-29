#voglio misurare la diversit� in pop
#ho popsize righe di permutazioni della serie da 1:nJobs
#pi� sono simili tra loro le righe, pi� il risultato della somma in colonne
#sar� distante dal valore atteso
#di una distibuzione uniforme di valori tra 1:nJobs, e tale valore sarebbe
nJobs=dim(pop)[2]
h0mean=mean(c(1:nJobs))*popsize
situation=colSums(pop)
distance=sums(abs(situation-h0mean))/h0mean
max_dist=sums(abs(((c(1:nJobs)*popsize) - h0mean)))/h0mean
treshold=inbred*max_dist

immigrationcount=0
#se la distanza dal casuale � troppa, cio� la similarit� fra righe/individui � alta
#allora introduco dei membri casuali
if(distance>=treshold){
  pop=rbind(pop, generateRandomPopulation(...))
  migrationcount=immigrationcount+1
}

#ora mi serve un modo per mantenere comunque almeno il singolo migliore, perch�
#ho perso un nuovo massimo
#direi di metterlo al post del peggiore, ma who cares in realt�


#tentiamo
genalgo<-function(popsize, epoche, mutation=FALSE, mutationRate=0, diversify=FALSE, inbred=0){
  pop = generateRandomPopulation(popsize) #inizializza popolazione
  scores = evaluatePopulation(pop) #inizializza fitness nella popolazione
  history = data.frame(0,0,0,0)
  colnames(history)<-c("gen", "best", "avg", "median")
  
  if(diversify){
    nJobs=dim(pop)[2]
  h0mean=mean(c(1:nJobs))*popsize #diversit� attesa
  situation=colSums(pop) #situazione effettiva
  distance=sum(abs(situation-h0mean))/h0mean #diversit� effettiva
  max_dist=sum(abs(((c(1:nJobs)*popsize) - h0mean)))/h0mean #minima diversit�
  treshold=inbred*max_dist #soglia permessa
  
  immigrationcount=0
  }
  
  
  for (gen in 1:epoche){
    offspring = matrix(NA, 1, nJobs)
    
    
    if(diversify){
      situation=colSums(pop) #situazione effettiva
      distance=sum(abs(situation-h0mean))/h0mean #diversit� effettiva
      if(distance>=treshold){
        pop=rbind(pop, generateRandomPopulation(popsize))
        immigrationcount=immigrationcount+1
        
        Stats=populationStats(gen, scores)
        
        best=pop[1,]
        for(i in 1:nrow(pop)){
          if(fitness(best)==Stats$best){
            best = pop[i,]
            break}
          }
        pop=pop[sample(c(1:(nrow(pop))), size=popsize-1, replace=FALSE),]
        pop=rbind(pop, best)
      }#migrazione e mixing
      
    }#post-miigrazione
    popsize = nrow(pop)
    for (i in 1:popsize){
      couple<-rouletteSelection(scores, pop) #"selezione sessuale": 
      #fitter, likelier to reproduce
      child<-generateOffspring(couple[1,], couple[2,])
      if (mutation){
        GeneDice<-runif(1,0,1)
        if(GeneDice<=mutationRate){
          child<-swapMutation(child)
        }
      }
      offspring<-rbind(offspring, child)
    } #fine processo di generazione
    
    temp_pop = rbind(pop, offspring[2:nrow(offspring),]) #popolazione di "vecchi" e nuova prole
    scores = evaluatePopulation(temp_pop) 
    Stats=populationStats(gen, scores)
    
    j=1
    for (i in 1:(popsize*2)){
      
      if (evaluateSchedule(fitness,temp_pop[i,]) > Stats$median ){
        pop[j,]<-temp_pop[i,]
        j=j+1
      } #solo la met� "migliore" della popolazione sopravvive alla prossima epoca
    } #fine "selezione naturale"
    
    
    
    scores = evaluatePopulation(pop)
    Stats=populationStats(gen, scores)
    history=rbind(history, c(Stats$gen, Stats$best, Stats$avg, Stats$median) )
  } #fine dell'evoluzione
  
  best=pop[1,]
  for(i in 1:nrow(pop)){
    if(fitness(best)==Stats$best){
      best = pop[i,]
      break
    }
  }
  
  return(list(Stats = Stats,
              history = history,
              BestMakespan = 1/(Stats$best),
              PossibleBest = best,
              LastGeneration = pop,
              immigrations = immigrationcount
  ) #list end
  )#return
  
}#end of function

