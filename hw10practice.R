n = 10^5
A = matrix(NA, N, 10);
V = matrix(NA, N, 10);
l.hat = matrix(NA, 1, 10);
for(j in 1:10){
  A[,j] = rnorm(N, mean = 0, sd = 1)
for (i in 1:N) {
  if (A[i,j] >= qnorm(1-P)) {
    V[i,j] = 1
  }
  else if (A[i,j] <= qnorm(P)) {
    V[i,j] = 1
  }
  else {
    V[i,j] = 0
  }
}
  for (k in 1:10) {
    l.hat[,k] = mean(V[,k])
  }
}