N=5000;
X = matrix(NA, N, 1);
sigma = 0.1;
mu = 10;
X[1] = runif(1)*sigma;
for(i in 1:N) {
  u <- runif(1);
  Y <- X[i] + u*sigma;
  if(u <= min(exp(-0.5*(Y-10)^2 + 0.5*(X[i]-10)^2), 1)) {
    X[i+1] = Y
  }
  else {
    X[i+1] = X[i]
  }
}
plot(1:N, X[1:N], type ="l", xlab = "Number of Iterations", ylab = "State of Xt")

x = matrix(NA, N, 1);
x[1] = 0;
for(i in 1:N) {
  z <- rnorm(1, mu, sigma^2)
  y <- x[i] + z
  u <- runif(1)
  if(u <= min(exp(-0.5*(y-10)^2 + 0.5*(x[i]-10)^2), 1)) {
    x[i+1] = y
  }
  else{
    x[i+1] = x[i]
  }
}
plot(1:N, x[1:N], type ="l", xlab = "Number of Iterations", ylab = "State of Xt")
