#Part 1: asymptotic CI for proportions:


#Wat Dem eg

x <- 112 #successes
n<-300 #trials

phat <- x/n

phat

CIlev <- .98

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
zinCI <- qnorm(oneminusalphaovertwo)

zinCI

LowP <- phat - zinCI * sqrt(phat* (1-phat)/n)
UpP <- phat + zinCI * sqrt(phat *(1-phat)/n)

CI <- c(LowP,UpP)

cat("The asymptotic", CIlev*100, " % confidence interval for p is", CI) #concatenate

prop.test(x,n,conf.level=CIlev,correct=TRUE) #preferred CI with correction...use professionally

####Part 2 ch 19

#Solve for sample size:

phat <- .46  #use .5 if not given in the problem
bound <- .05
CIlev <- .9

alpha <- 1-CIlev
alphaovertwo <- alpha/2
oneminusalphaovertwo <- 1- alpha/2
zinCI <- qnorm(oneminusalphaovertwo)

zinCI

n <- (zinCI/bound)^2*phat*(1-phat)

n

cat("The required sample size is ",ceiling(n))  #ceiling function always goes up to the next integer



#Solve for confidence level:

bound <- .03
n <- 1507
phat <- .49

zsolved <- bound *sqrt(n/(phat*(1-phat)))

zsolved

arealess <- pnorm(zsolved)

#arealess = 1 - alpha/2
#alpha/2 = 1 - arealess
#alpha = 2*(1-arealess)
#Conf = 1-alpha

Conf <- (1 - 2*(1-arealess))*100

cat("This is a ",Conf,"% confidence interval.")

