#DEFINIZIONI FUNZIONI
library(dplyr)
library(pracma)
library(curry)
library(GA)
library(ggplot2)

set.seed(13)

MakeSpan<-function(ProcTimes){
  CompTimes<-matrix(NA, nrow = dim(ProcTimes)[1], ncol = dim(ProcTimes)[2])
  CompTimes[1,]<-ProcTimes[1,]
  for(i in 2:dim(ProcTimes)[1]){
    CompTimes[i,1]<-CompTimes[i-1,1]+ProcTimes[i,1]
    for (j in 1:dim(ProcTimes)[2]){
      CompTimes[i,j] <- max(CompTimes[i-1,j],CompTimes[i,j-1])+ProcTimes[i,j]
    }#end of col cycle
  }#end of row cycle
  makespan<-CompTimes[dim(CompTimes)[1],dim(CompTimes)[2]]
  return(makespan)
}



#Fitness
FSSPfitness<-function(Schedule, ProcTimes) {
  
  ProcTimes[,]<-ProcTimes[Schedule,]
  jobs_rownames<-paste0("job", Schedule)
  
  return(1/MakeSpan(ProcTimes))
}

fitness<-function(Schedule){FSSPfitness(Schedule, ProcTimes)}


#Generate, Evaluate a Population
nJobs = nrow(ProcTimes)
#to generate a schedule
generateRandomSchedule <- function(){ vet <- c(1:nJobs)
randperm(vet) } 
#to generate a population
generateRandomPopulation <- function(popSize){t(replicate(popSize,
                                                          generateRandomSchedule()))}
#to evaluate fitness of schedule
evaluateSchedule <- function(fitFun, Schedule){fitFun(Schedule)}

#to evaluate a population
evaluatePopulation <- function(pop){
  data.frame(index =c(1:nrow(pop)), fit = apply(pop, 1,
                                                curry(evaluateSchedule,fitness)))
}

#to check
populationStats <- function(gen, scores){
  bestScore <- max(scores$fit)
  avgScore <- mean(scores$fit)
  medianScore <- median(scores$fit)
  item = list(gen = gen, best = bestScore, avg = avgScore, median =medianScore )
  class(item)<- "traceItem"
  return(item)
}

#fitness proportion selection
rouletteSelection <- function(scores, pop){
  idx= sample_n(scores, size = 2, replace = F, weight = scores$fit)$index
  pop[idx, ]
}

#crossover
generateOffspring <- function(p1, p2){
  swath = floor(length(p1)/2)
  cut1 = sample(nJobs-swath, 1)
  cut2 = cut1+swath-1
  off1 = rep(0, nJobs)
  sel = p1[cut1:cut2]
  off1[cut1:cut2]= sel
  r = p2[!p2 %in% sel] # not the selected and copied ones
  r1= r[c(1:cut1-1)]
  r2= r[c(cut1:length(r))]
  off1[c(1:cut1-1)] = r1
  off1[c((cut2+1):length(off1))] = r2
  off1
}

#mutation
swapMutation <- function(p){
  pos = sample(nJobs, 2)
  p[c(pos[1], pos[2])] = p[c(pos[2], pos[1])]
  p
}

#ALL
genalgo<-function(popsize, epoche, mutation=FALSE, mutationRate=0, diversify=FALSE, inbred=0){
  pop = generateRandomPopulation(popsize) #inizializza popolazione
  scores = evaluatePopulation(pop) #inizializza fitness nella popolazione
  history = data.frame(0,0,0,0)
  colnames(history)<-c("gen", "best", "avg", "median")
  migrationcount=NULL
  if(diversify){
    geni=ncol(pop)
    individui=nrow(pop)
    h0mean=mean(c(1:geni))*individui #diversità attesa
    situation=colSums(pop) #situazione effettiva
    distance=sum(abs(situation-h0mean)/h0mean) #diversità effettiva
    max_dist=sum(abs(((c(1:geni)*individui) - h0mean)/h0mean)) #minima diversità
    treshold=inbred*max_dist #soglia permessa
    
    migrationcount=0
  }
  
  
  for (gen in 1:epoche){
    offspring = matrix(NA, 1, nJobs)
    
    
    if(diversify){
      situation=colSums(pop) #situazione effettiva
      distance=sum(abs(situation-h0mean)/h0mean) #diversità effettiva
      if(distance>=treshold){
        mixedpop=rbind(pop, generateRandomPopulation(popsize))
        migrationcount=migrationcount+1
        newscores = evaluatePopulation(mixedpop)
        newStats=populationStats(gen, newscores)
        
        fittest=mixedpop[1,]
        for(i in 1:nrow(mixedpop)){
          if(fitness(mixedpop[i,])==newStats$best){
            fittest = mixedpop[i,]
            break}
        }
        pop=mixedpop[sample(c(1:(nrow(mixedpop))), size=popsize-1, replace=FALSE),]
        pop=rbind(pop, fittest)
      }#migrazione e mix
      
    }#post-migrazione
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
    for (i in 1:nrow(temp_pop)){
      
      if (fitness(temp_pop[i,]) > Stats$median ){
        pop[j,]<-temp_pop[i,]
        j=j+1
      } #solo la metà "migliore" della popolazione sopravvive alla prossima epoca
    } #fine "selezione naturale"
    
    
    
    scores = evaluatePopulation(pop)
    Stats=populationStats(gen, scores)
    history=rbind(history, c(Stats$gen, Stats$best, Stats$avg, Stats$median) )
  } #fine dell'evoluzione
  
  best=pop[1,]
  for(i in 1:nrow(pop)){
    if(fitness(pop[i,])==Stats$best){
      best = pop[i,]
      break
    }
  }
  
  if(diversify){
    BiodiversityStats=c(distance, max_dist, treshold)
  } else {
      BiodiversityStats=NULL
      }
  return(list(Stats = Stats,
              history = history,
              BestMakespan = 1/(Stats$best),
              PossibleBest = best,
              LastGeneration = pop,
              migrations = migrationcount,
              BioDiversity = BiodiversityStats
  ) #list end
  )#return
  
}#end of function

evolutionaryTale<-function(Object){
  history<-Object$history
  ggplot(history, aes(history$gen)) + 
    geom_point(aes(y = history$best, colour = "best fitness")) + 
    geom_point(aes(y = history$median, colour = "median fitness")) +
    xlab("Generation")+ylab("Fitness")
}