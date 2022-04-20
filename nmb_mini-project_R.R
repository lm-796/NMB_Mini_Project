rm(list = ls())  #this line of code will clean yor working environment 

# Set working directory and importing dataset in R
setwd("C:/OFFICE_/NMB_quick_miniproject")
nmbdata<-read.delim("nmb_miniproject.txt", head=T)

nmbdata<-read_excel("exammock_MA7512.xlsx",sheet = "question 2")
head(nmbdata) 

##The dependent variable is binary, therefore a generalised linear model under Binomial family will be appropriate 
#install.packages("lme4") #By removing the LHS hash will install the required package (if a need be)
library(lme4) #An appropriate package for Linear, generalized linear, and nonlinear mixed models

##Fit model with all covariates and factors but without random effects
##In model names, after the word "model" the next letters represents the covariates included in each case
modelilt<-glm(complete~intensity+length+factor(training),data = nmbdata, family=binomial())
modelil<-glm(complete~intensity+length,data = nmbdata, family=binomial())
modelit<-glm(complete~intensity+factor(training),data = nmbdata, family=binomial())
modeli<-glm(complete~intensity,data = nmbdata, family=binomial())
modellt<-glm(complete~length+factor(training),data = nmbdata, family=binomial())
modell<-glm(complete~length,data = nmbdata, family=binomial())
modelt<-glm(complete~factor(training),data = nmbdata, family=binomial())
modelconst <- glm(complete~ 1,data = nmbdata,family = binomial()) # a model with constant only

##AIC table

AICvalues <- c(AIC(modelilt),AIC(modelil),AIC(modelit),AIC(modeli),AIC(modellt),AIC(modell),AIC(modelt),AIC(modelconst))
modelname <- c("intensity+length+training", "intensity+length", "intensity+training","intensity","length+training","length","training", "Const")

AICvaluesr <- round(AICvalues,digits=2)
deltaAIC <-AICvalues-min(AICvalues)
deltaAICr <-round(deltaAIC,digits=2)
AICweight <- round(exp(-deltaAIC/2)/sum(exp(-deltaAIC/2)),digits=2)
AICtable <- data.frame(modelname,AICvaluesr,deltaAICr,AICweight)
colnames(AICtable ) <- c("Model", "AIC","Delta AIC","AIC weight")
print(AICtable)


##Level 1 Analysis of Deviance - compare to constant model
Modellist <- list( )
Modellist[[1]] <- modeli
Modellist[[2]] <- modell
Modellist[[3]] <- modelt
modelcompare <- modelconst
modelname <- c("intensity", "length","training") 

## Create Analysis of Deviance 
dev <- rep(0,(length(Modellist)))
for (i in 1:(length(Modellist))){dev[i]=Modellist[[i]]$deviance}
devcompare <- modelcompare$deviance
DF <- rep(0,(length(Modellist)))
for (i in 1:(length(Modellist))){DF[i]=Modellist[[i]]$df.residual}
DFcompare <- modelcompare$df.residual
diffdev=devcompare-dev
diffDF=DFcompare-DF
pval=pchisq(diffdev, df=diffDF, lower.tail=FALSE)
devtable = data.frame(modelname,diffdev,diffDF,pval)
colnames(devtable) <- c("Model", "Diff Deviance","Diff DF","p-value")
print(devtable)

##Add second level
Modellist <- list( )
Modellist[[1]] <- modelil
Modellist[[2]] <- modelit
modelcompare <- modeli
modelname <- c("intensity+length", "intensity+training") 

## Create Analysis of Deviance
dev <- rep(0,(length(Modellist)))
for (i in 1:(length(Modellist))){dev[i]=Modellist[[i]]$deviance}
devcompare <- modelcompare$deviance
DF <- rep(0,(length(Modellist)))
for (i in 1:(length(Modellist))){DF[i]=Modellist[[i]]$df.residual}
DFcompare <- modelcompare$df.residual
diffdev=devcompare-dev
diffDF=DFcompare-DF
pval=pchisq(diffdev, df=diffDF, lower.tail=FALSE)
devtable = data.frame(modelname,diffdev,diffDF,pval)
colnames(devtable) <- c("Model", "Diff Deviance","Diff DF","p-value")
print(devtable)


## Now we are in a position to consider random effects
modelilp <- glmer(formula=complete~ intensity+length+(1|Participant),data = nmbdata,family = binomial()) #Note: ignore the error boundary (singular) fit: see help('isSingular')
## LRT
LRT <- 2*(logLik(modelilp)[1]-logLik(modelil)[1])
pvalue <- 1-pchisq(LRT, df=1) 
LRTresults <- data.frame(LRT,pvalue)
print(LRTresults)


modelils <- glmer(formula=complete~ intensity+length+(1|Staff),data = nmbdata,family = binomial()) #Note: ignore the error boundary (singular) fit: see help('isSingular')
## LRT
LRT <- 2*(logLik(modelils)[1]-logLik(modelil)[1])
pvalue <- 1-pchisq(LRT, df=1) 
LRTresults <- data.frame(LRT,pvalue)
print(LRTresults)


##AIC
#install.packages("lmerTest") #remove the LHS hash to install (if a need be)
library(lmerTest)
AIC(modelilt,modelil,modelit,modeli,modellt,modell,modelt,modelconst,modelils,modelilp)
  
  
  
