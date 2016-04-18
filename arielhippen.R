library(hash)
library(pryr)
getUniquePerms <- function(groupAssignments, numPerms)
{
  set.seed(1)
  cases<-length(groupAssignments[which(groupAssignments==1)])
  total<-length(groupAssignments)
  browns<-hash()
  possible<-choose(total,cases)
  if (possible<numPerms) {
    numPerms=possible
  }
  permsLeft=numPerms
  while(permsLeft>0){
    for(i in 1:permsLeft){
      vec<-sample(groupAssignments,replace=FALSE)
      ones<-which(vec==1)
      perm<-paste(ones, collapse = ' ')
      browns[[perm]]<-0
    }
    permsLeft<-numPerms-length(browns)
  }
  keys(browns)
}

caseControlStatus<-c(1,1,1,1,1,0,0,0,0,0)

test1<-getUniquePerms(caseControlStatus,100)
test2 <- getUniquePerms(caseControlStatus, 10000)

caseControlStatusBig <- c(rep(1, 100), rep(0, 900))
mem_used()
mem_change(z<-system.time(test3 <- getUniquePerms(caseControlStatusBig, 100000)))

length(unique(test3))