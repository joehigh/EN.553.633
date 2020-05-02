#Problem 2.20, Part (d)

f<-function(x, a=1){exp(-x)/(1-exp(-a))} #defining the truncated exponential function
g<-function(x){dexp(x, rate=1)}
set.seed(1)
n=20000  #number of sample points to generate
prop.func<-rexp(n)  #generating exponentially distributed random variables
U<-runif(n)  #generating uniform random variables
C<-1/(1-exp(-1)) #optimal C from part (b)
U.a<- U < f(prop.func)/(C*g(prop.func)) 
accept<-prop.func[U.a]  #accepted points
proportion<-length(accept)/n  #Proportion is 1 since the f and C*g cancel out when a=1.
x.accept<-sort(accept)
x<-seq(-10,10, by=0.1)
hist(x.accept, xlim=c(0, 10), prob=T, main = "Histogram with Trunc Exp Overlay")
lines(x, f(x), col="blue")

#The proportion of accepted points is 1 when a=1. This can also be seen in the histogram plot.
#The true truncated exponential distribution function lies over the histogram, suggesting that 
#it has accepted all points. The theoretical proportion of accepted points is 0.632
#(the efficiency). However, when this theoretical proportion is computed, it is being computed
#for all values of a. Here, we are only considering a=1 and this value of a along with the 
#optimal value of C results in the proposal function*C = truncated exponential function. 
#Hence, the proportion of 1.