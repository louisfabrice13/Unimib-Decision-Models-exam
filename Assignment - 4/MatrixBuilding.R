#matrix building
datamat<-matrix(data=c(54, 83, 15, 71, 77, 36, 53, 38, 27, 87, 76, 91, 14, 29, 12, 77, 32, 87, 68, 94,
                       79,  3, 11, 99, 56, 70, 99, 60,  5, 56,  3, 61, 73, 75, 47, 14, 21, 86,  5, 77,
                       16, 89, 49, 15, 89, 45, 60, 23, 57, 64,  7,  1, 63, 41, 63, 47, 26, 75, 77, 40,
                       66, 58, 31, 68, 78, 91, 13, 59, 49, 85, 85,  9, 39, 41, 56, 40, 54, 77, 51, 31,
                       58, 56, 20, 85, 53, 35, 53, 41, 69, 13, 86, 72,  8, 49, 47, 87, 58, 18, 68, 28), nrow=5, ncol=20, byrow=TRUE)


datamat<-as.matrix(cinquecJventiM)
jobs_colnames<-paste0("job", 1:dim(datamat)[2])
machines_rownames<-paste0("machine", 1:dim(datamat)[1])
colnames(datamat)<-jobs_colnames
rownames(datamat)<-machines_rownames

#makespan
ProcTimes <- t(datamat)
nJobs=dim(ProcTimes)[1]
#per ogni job, l'operazione i-esima va sulla macchina i-esima, appena si libera

CompTimes<-matrix(NA, nrow = dim(ProcTimes)[1], ncol = dim(ProcTimes)[2])
CompTimes[1,]<-ProcTimes[1,]
for(i in 2:dim(ProcTimes)[1]){
  CompTimes[i,1]<-CompTimes[i-1,1]+ProcTimes[i,1]
  for (j in 1:dim(ProcTimes)[2]){
    CompTimes[i,j] <- max(CompTimes[i-1,j],CompTimes[i,j-1])+ProcTimes[i,j]
  }#end of col cycle
}#end of row cycle
makespan<-CompTimes[dim(CompTimes)[1],dim(CompTimes)[2]]


#as a single function
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
