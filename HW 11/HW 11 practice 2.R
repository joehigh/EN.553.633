n = 5000;
mu = 10;
sigma = 0.1;

x = matrix(NA, n, 1);
x[1] = 0;
for(i in 1:n) {
  z <- rnorm(1, 0, sigma)
  y <- x[i] + z
  u <- runif(1)
  f <-dnorm(y, mu, 1)
  g <-dnorm(x[i], mu, 1)
  if(u <= min(f/g, 1)) {
    x[i+1] = y
  }
  else{
    x[i+1] = x[i]
  }
}
plot(1:n, x[1:n], type ="l", xlab = "Number of Iterations", ylab = "State of Xt")
