library(hash)
library(pryr)

getUniquePerms <- function(groupAssignments, numPerms)
{
  set.seed(1)
  cases<-length(groupAssignments[which(groupAssignments==0)])
  browns<-hash()
  total<-length(groupAssignments)
  possible<-choose(total,cases)
  while(length(browns)<numPerms && length(browns)<possible){
    vec<-rep(0,length=total)
    x<-sample(1:total,cases,replace=FALSE)
    vec[x]<-1
    perm<-paste(vec, collapse = '')
    if(!has.key(perm,browns)){
      browns[[perm]]<-0
    }
  }
  keys(browns)
}

caseControlStatus<-c(1,1,1,1,1,0,0,0,0,0)

test1<-getUniquePerms(caseControlStatus,100)
test2 <- getUniquePerms(caseControlStatus, 10000)

caseControlStatusBig <- c(rep(1, 100), rep(0, 900))
mem_used()
mem_change(z<-system.time(test3 <- getUniquePerms(caseControlStatusBig, 1000)))

length(unique(test3))



old<-c(4.37,5.15,6.17)
oldspace<-c(184kb)

