library(hash)
library(pryr)
getUniquePerms <- function(groupAssignments, numPerms)
{
  set.seed(1)
  cases<-length(groupAssignments[which(groupAssignments==1)])
  size<-length(groupAssignments)
  hashTable<-hash()
  possible<-choose(size,cases)
  if (possible<numPerms) {
    numPerms=possible
  }
  permsLeft=numPerms
  while(permsLeft>0){
    for(i in 1:permsLeft){
      shuffledVector<-sample(groupAssignments,replace=FALSE)
      caseIndices<-which(shuffledVector==1)
      perm<-paste(caseIndices, collapse = ' ')
      hashTable[[perm]]<-0
    }
    permsLeft<-numPerms-length(hashTable)
  }
  keys(hashTable)
}

caseControlStatus<-c(1,1,1,1,1,0,0,0,0,0)

test1<-getUniquePerms(caseControlStatus,100)
test2 <- getUniquePerms(caseControlStatus, 10000)

caseControlStatusBig <- c(rep(1, 100), rep(0, 900))
mem_used()
mem_change(z<-system.time(test3 <- getUniquePerms(caseControlStatusBig, 100000)))

length(unique(test3))