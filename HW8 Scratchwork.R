M<-10^4; #simulation time
t.clock <- 0;
lambda <- 1;
mu <- 2;
t1 <- 0;
t2 <- M
tn<-t.clock
tb<-0
n<-0
s<-0
b<-0
c<-0
qc<-0
tc<-0
plotSamples<-100
set.seed(1)

while (t.clock < M) {
  if (t1 < t2) {      # arrival event
    t.clock <- t1
    s <- s + n * (t.clock - tn)  # delta time-weighted number in queue
    n <- n + 1
    if (t.clock < plotSamples) { 
      qc <- append(qc,n)
      tc <- append(tc,t.clock) 
    }
    tn <- t.clock
    t1 <- t.clock + rexp(1, 1/lambda)
    if(n == 1) { 
      tb <- t.clock
      t2 <- t.clock + rexp(1, 1/mu)  # exponential  interarrival period
    }
  } else {            # departure event
    t.clock <- t2
    s <- s + n * (t.clock - tn)  # delta time-weighted number in queue
    n <- n - 1
    if (t.clock < plotSamples) { 
      qc <- append(qc,n)
      tc <- append(tc,t.clock)
    }
    tn <- t.clock
    c <- c + 1
    if (n > 0) { 
      t2 <- t.clock + rexp(1, 1/mu)  # exponential  service period
    }
    else { 
      t2 <- M
      b <- b + t.clock - tb
    }
  }   
}

u <- b/t.clock       # utilization B/T
N <- s/t.clock       # mean queue length (see the Load Average notes)
x <- c/t.clock       # mean throughput C/T
r <- N/x             # mean residence time (from Little's law: Q = XR)
q <- sum(qc)/max(tc) # estimated queue length for plot
