#### Gustavo Brusse - EDSD 2018/2019 ###
#### Assignment Event History 3 ########

## Exercise 1

## 1.1 creating a data.frame for the data

Survivors<-c(20,192,1,5,140,57,13,11,80,14,14,13,76,75)
Total.number<-c(23,862,1,5,144,175,13,11,93,168,31,48,165,462)
Class<-factor(c(0,0,1,1,1,1,2,2,2,2,3,3,3,3), labels=c("Crew", "First", "Second", "Third"))
Age<-factor(c(0,0,1,1,0,0,1,1,0,0,1,1,0,0), labels=c("Adult", "Child"))
Sex<-factor(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1), labels=c("Female", "Male"))

titanic<-data.frame(Survivors,Total.number,Class,Age,Sex)
head(titanic)

## 1.2 Yes, we have only information about peolple that survived. 
## But we need to have information about the people that died, as the event is "die". 

titanic$Died<-titanic$Total.number-titanic$Survivors

## 1.3 Best suit model

fullmod<-glm(formula = cbind(Survivors, Died) ~ Class+Age+Sex, family = binomial(link = logit), data=titanic)
step(fullmod)

head(titanic)

## using the stepmodel (backwards) we find the model that best suit the data is composed
## with variables Calss, Age, Sex

## 1.4 Best suit model summary

m1<-glm(formula = cbind(Survivors, Died) ~ Class+Age+Sex, family = binomial(link = logit), data=titanic)
summary(m1)
par(mfrow=c(2,2))
plot(m1)


## 1.5 

Fem<-matrix(NA,nrow=2,ncol=4)
Fem[1,1] <-titanic$Died[which(titanic$Class=="Crew" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[1,2] <-titanic$Died[which(titanic$Class=="First" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[1,3] <-titanic$Died[which(titanic$Class=="Second" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[1,4] <-titanic$Died[which(titanic$Class=="Third" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[2,1] <-titanic$Survivors[which(titanic$Class=="Crew" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[2,2] <-titanic$Survivors[which(titanic$Class=="First" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[2,3] <-titanic$Survivors[which(titanic$Class=="Second" & titanic$Age=="Adult" & titanic$Sex=="Female")]
Fem[2,4] <-titanic$Survivors[which(titanic$Class=="Third" & titanic$Age=="Adult" & titanic$Sex=="Female")]
colnames(Fem)<-c("Crew","First","Second","Third");Fem
Mal<-matrix(NA,nrow=2,ncol=4)
Mal[1,1] <-titanic$Died[which(titanic$Class=="Crew" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[1,2] <-titanic$Died[which(titanic$Class=="First" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[1,3] <-titanic$Died[which(titanic$Class=="Second" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[1,4] <-titanic$Died[which(titanic$Class=="Third" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[2,1] <-titanic$Survivors[which(titanic$Class=="Crew" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[2,2] <-titanic$Survivors[which(titanic$Class=="First" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[2,3] <-titanic$Survivors[which(titanic$Class=="Second" & titanic$Age=="Adult" & titanic$Sex=="Male")]
Mal[2,4] <-titanic$Survivors[which(titanic$Class=="Third" & titanic$Age=="Adult" & titanic$Sex=="Male")]
colnames(Mal)<-c("Crew","First","Second","Third");Mal

# Barplots
par(mfrow=c(1,2))
barplot(Fem/sum(Fem+Mal), main="Females", col=1:2,ylim=c(0,0.2))
barplot(Mal/sum(Fem+Mal), main="Males", col=1:2,ylim=c(0,0.48))
legend("topright", c("Death", "Survive"), col=1:2, pch=15)

## Exercise 2

## Reading the data set 

data <- read.table("students.txt", header=TRUE)
data$dur<-as.numeric(data$dur)
data$sex<-as.factor(data$sex)
data$subj<-as.factor(data$subj)

## 2.1 Frequency table

table(data$dur[which(data$event==TRUE)]) 
table(data$dur[which(data$event==FALSE)])
mean(data$dur)
median(data$dur)

## barplot

barplot(table(data$dur[which(data$event==TRUE)]),main="Frequency of duration variable",xlab="Duration",
        ylab="Frequency") 
t2<-c(rep(0,24),table(data$dur[which(data$event==FALSE)]))
barplot(t2,col="red",add=T,names.arg=c(1:24,">24"))
legend("topleft", c("Event", "Censored"), col=c("grey","red"), pch=15)

## 2.2 Quarter varible

data$quarter <-as.numeric(cut(data$dur, breaks=seq(0,24,3)))
head(data)

## Preparing data for Discrete Time Models 

n=290
data$indiv <- 1:n

i <- 1
data2 <- cbind(rep(data$indiv[i], data$quarter[i]),
              rep(data$quarter[i], data$quarter[i]),
              rep(data$sex[i], data$quarter[i]),
              rep(data$subj[i], data$quarter[i]),
              1:data$quarter[i],
              c(rep(0, data$quarter[i]-1), data$event[i]))
for(i in 2:nrow(data)){
  data2 <- rbind(data2,
                cbind(rep(data$indiv[i], data$quarter[i]),
                      rep(data$quarter[i], data$quarter[i]),
                      rep(data$sex[i], data$quarter[i]),
                      rep(data$subj[i], data$quarter[i]),
                      1:data$quarter[i],
                      c(rep(0, data$quarter[i]-1), data$event[i])
                ))
}

data2 <- as.data.frame(data2)
names(data2) <- c("indiv", "t.disc", "sex", "subj", "time.new", "status")

data2$time.new <- as.factor(data2$time.new)
data2$sex<-as.factor(data2$sex)
data2$subj<-as.factor(data2$subj)

dim(data)
dim(data2)
head(data2)


###### logistic 
# baseline only 
logistic.0 <- glm(status ~ -1 +time.new, family=binomial, data=data2)
summary(logistic.0)

#  adding sex, subject and the interaction
logistic.1 <- glm(status ~-1 + time.new+sex*subj, family=binomial, data=data2)
summary(logistic.1)

###### c-loglog 
# baseline only  
cloglog.0 <- glm(status ~ -1 + time.new, family=binomial(link=cloglog),
                 data=data2)
summary(cloglog.0)

# adding sex, subject and the interaction
cloglog.1 <- glm(status ~ -1 + time.new+sex*subj, family=binomial(link=cloglog), data=data2)
summary(cloglog.1)

## ploting the hazard

bs.log <- logistic.0$coef
h.log <- exp(logistic.0$coef) / (1 + exp(logistic.0$coef))
bs.clog <- cloglog.0$coef
h.clog <- 1 - exp(-exp(cloglog.0$coef))
x=1:8
plot(x, h.log, cex=2, main="Discrete baseline hazard",
     xlab="Time", ylab="Discrete Hazard",
     ylim=c(0,1))
points(x, h.clog, cex=2, col=2, pch=2)
true.haz <- (pweibull(x, shape=2, scale=15) -
               pweibull(x-1, shape=2, scale=15))/
  pweibull(x-1, shape=2, scale=15, lower.tail=F)
points(x, true.haz, col=3, pch=3, cex=2)
legend("topright",legend=c("logistic", "cloglog", "true"),
       col=1:3, pch=1:3)

## Exercise 3

deaths<-read.table("SWIdeaths.txt", header=TRUE,sep=" ")
exposures<-read.table("SWIexposures.txt", header=TRUE,sep=" ")

## 3.1
lnLmakeham <- function(theta, ages, Y, E){
  # parameters
  a <- theta[1]
  b <- theta[2]
  c <- theta[3]
  # Makeham mu
  mu <- c + a * exp(b * ages)
  llk <- - sum(Y * log(mu) - mu*E)
}

ages<-0:100
ages3090<-30:90
dea50<-deaths$X1950[31:91]
exp50<-exposures$X1950[31:91]

st.val <- c(0.00002,0.1,0.0009)
fit <- optim(par=st.val,
             fn=lnLmakeham,
             ages=ages3090, Y=dea50, E=exp50)

# 3.2
a.hat <- fit$par[1]
b.hat <- fit$par[2]
c.hat <- fit$par[3]
library(nlme)
Hessian <- fdHess(pars=fit$par, fun=lnLmakeham,
                  ages=ages3090, Y=dea50, E=exp50)$Hessian
vcmat <- solve(Hessian)
CIs <- data.frame(estimate=c(a.hat,
                             b.hat,
                             c.hat),
                  lower=c(a.hat-qnorm(.975)*sqrt(vcmat[1,1]),
                          b.hat-qnorm(.975)*sqrt(vcmat[2,2]),
                          c.hat-qnorm(.975)*sqrt(vcmat[3,3])),
                  upper=c(a.hat+qnorm(.975)*sqrt(vcmat[1,1]),
                          b.hat+qnorm(.975)*sqrt(vcmat[2,2]),
                          c.hat+qnorm(.975)*sqrt(vcmat[3,3])))
rownames(CIs) <- c("a", "b", "c")
CIs 

# 3.3
mu.hat <- c.hat + a.hat * exp(b.hat * ages3090)
plot(ages3090, dea50/exp50, cex=1.2, ylab="mu",
     main="Rates",ylim=c(0,0.3),xlab="Age")
lines(ages3090, mu.hat, col=4, lwd=3)
legend("topleft",
       legend=c("Observed", "Fitted Makeham"),
       col=c(1,4), lwd=c(1,1), lty=c(-1,1),
       pch=c(1,-1))

# 3.4 
deathsM<-as.matrix(read.table("SWIdeaths.txt", header=TRUE,sep=" "))
exposuresM<-as.matrix(read.table("SWIexposures.txt", header=TRUE,sep=" "))

source("FUNCTIONSleecarter.r")

fitLC <- LCpoi(Dth=deathsM, Exp=exposuresM)
names(fitLC)
a.LC <- fitLC$Alpha
b.LC <- fitLC$Beta
k.LC <- fitLC$Kappa
One <- matrix(1, nrow=58, ncol=1)
MU.LC <- exp(a.LC %*% t(One) + b.LC %*% t(k.LC))
D.LC <- MU.LC * exposures

# 3.5
par(mfrow=c(1,3))
plot(ages, a.LC, t="l", lwd=3,
     ylab=" ",xlab="Age",main="Alpha")
plot(ages, b.LC, t="l", lwd=3,
     ylab=" ",main="Beta",xlab="Age")
plot(1950:2007, k.LC, t="l", lwd=3,
     ylab=" ",xlab="Year",main="Kappa")
par(mfrow=c(1,1))

# 3.6
years=1950:2007
par(mfrow=c(2,2))
for (i in c(1,31,61,91)){
  plot(years, log(deathsM[i,]/exposuresM[i,]), main=paste("Age",i-1),
       ylab="log-mortality")
  lines(years, log(MU.LC[i,]), col=2, lwd=3)
  points(1950,log(mu.hat[i]),pch=2,col=4)
  legend("bottomleft",
         legend=c("Observed", "Fitted Lee-Carter"),
         col=c(1,2,4), lwd=c(1,1,1), lty=c(-1,1,1),
         pch=c(1,-1,2),cex = 0.6)
}

