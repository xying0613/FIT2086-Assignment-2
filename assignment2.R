setwd("C:/Users/User/Documents/YEAR 2 SEM 2/FIT2086/Assignment/Assignment 2")
fuel <- read.csv("Fuel.efficiency.csv", header = TRUE)

a <- fuel[fuel$Type=="A",]
p <- fuel[fuel$Type=="P",]

mean(a$FA)
var(a$FA)
nrow(a)

qt(1-0.05/2, 20-1)

mean(p$FA)
var(p$FA)
nrow(p)

mean(a$FA)-qt(1-0.05/2, 20-1)*(sqrt(var(a$FA)/20))
mean(a$FA)+qt(1-0.05/2, 20-1)*(sqrt(var(a$FA)/20))

(10.46713-8.771594)+(1.96)*(sqrt((var(a$FA)/20)+(var(b$FA)/25)))
(10.46713-8.771594)/sqrt((7.559223/20)+(9.386937/25))

pnorm(1.9534)

gpmf0 <- function(y){
  ((exp(0)+1)^(-y-1)) * (exp(y*0))
}

gpmf1 <- function(y){
  ((exp(1)+1)^(-y-1)) * (exp(y*1))
}

gpmf2 <- function(y){
  ((exp(2)+1)^(-y-1)) * (exp(y*2))
}

plot()

curve(gpmf0, from=0, to=20, xlab="y", ylab="P(y|L)", main="Graph of geometric probability mass function given", col="red")
curve(gpmf1, from=0, to=20, add=TRUE, col="blue")
curve(gpmf2, from=0, to=20, add=TRUE, col="black")
legend(x=15, y=0.3, legend=c("L=0","L=1","L=2"), col=c("red","blue","black"), lwd=2, lty=c(1, 1))


(11/26 - 0.53)/sqrt(0.53*(1-0.53)/26)

(11/26)+(1.96*sqrt((11/26)*(1-11/26)/26))
2*pnorm(-1.0924)

res <- binom.test(11,26,0.53)
res
res$p.value

(11/26-20/26)+sqrt(((11/26)*(1-11/26)/26)+((20/26)*(1-20/26)/26))
2*pnorm(-0.2188)

2*pnorm(-1.4498)

3.6 - (1.96*sqrt(((11/26)*(1-(11/26))/26)))
         