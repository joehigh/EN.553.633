#Part (a) True Value of the Integral

#Defining the Function
f<-function(x){exp((-x^2)/2)}
#Integrating function from a=0 to b=1
integrate(f, lower = 0, upper = 1)
#True Value of integral for a=0, b=1: 0.856

#Integrating function from a=0 to b=4
integrate(f, lower = 0, upper = 4)
#True Value of integral for a=0, b=4: 1.253


#Part (b) Computing an estimate via the Monte Carlo technique

MC<-function(n,a,b){
U<-runif(n, a, b)
X<-exp((-U^2)/2)
MCI<-((b-a)/n)*sum(X)
}

## a=0, b=1
print(MC(20, 0, 1))  # n=20
print(MC(200, 0, 1)) # n=200
print(MC(2000, 0, 1)) # n=2000

## a=0, b=4
print(MC(20, 0, 4))  # n=20
print(MC(200, 0, 4)) # n=200
print(MC(2000, 0, 4)) # n=2000

#Part (c)
#As the number of random sample points increased, the smaller the distance between the 
#estimate and the true value. This is true for both combinations of a and b.