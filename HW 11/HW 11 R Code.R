set.seed(2)
N = 5000;
mu = 10;
sigma = 0.1;

X = matrix(NA, N, 1);  #storage for the X_t for all 5000 iterations
X[1] = 0;              #Initial value
for(i in 1:N) {
  z <- rnorm(1, mean = 0, sd = sigma)
  Y <- X[i] + z
  u <- runif(1)
  f.y <-dnorm(Y, mean = mu, sd = 1)
  f.x <-dnorm(X[i], mean = mu, sd = 1)
  alpha = min(f.y/f.x, 1)
  if(u <= alpha) {
    X[i+1] = Y
  }
  else{
    X[i+1] = X[i]
  }
}
plot(1:N, X[1:N], type ="l", xlab = "Number of Iterations t", ylab = "State of X_t") 
abline(v = 1000, col = "red", lty = "dashed")  #to visualize where stationarity approximately starts

#At approximately 1000 iterations (just as the solutions manual for the textbook suggests), the
#process appears to reach stationarity. Observe that for all t < 1000 (roughly), there is noticable
#correlation amongst the X_t. Indeed, for any t<1000, when X_t increases, X_{t+1} tends to increase 
#as well. Similarly, for t < 1000, when X_t decreases, X_{t+1} tends to decrease as well.
#However, for t > 1000 (roughly), the X_t become less and less correlated. That is, there is more 
#randomness in the state of X_t for each t > 1000. 

M = mcmc(data = X)
traceplot(M, type = "l")
