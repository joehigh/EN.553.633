statespace<-c(1,2,3)
P <- matrix(data = c(1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/5, 1/5, 3/5), nrow = 3, ncol = 3, 
            byrow = TRUE)
n=5000
xi1 = 0
xi2 = 0
xi3 = 0

ind<-matrix(data = c(0, 0, 0), nrow = 1, ncol = 3, byrow = TRUE)

for(i in 1:n) {
  x=1
  for (j in 1:20){
    for(k  in 1:3){
    if(x==k)
    x = sample(statespace, size = 1 , replace = FALSE, prob = P[k,])
    }
    else {
      x = sample(statespace, size = 1 , replace = FALSE, prob = P[k,])
    }
  }

for(t in 1:2)
if (x == t) {
   ind[,t] = ind[,t] + 1
}
  else {
    ind[,t] = ind[,t] + 1
  }
}

for(t in 1:3){
print(ind[,t]/n)
}
