
#### testing the SE !
NUMBI <- 5

ParList <- lapply(1:NUMBI,function(x)
{
  Item1 <- c(c(-2,-1,1,2),c(-1.2,0.3,0.2,0.7))
  names(Item1) <- c(paste("zeta",1:4,sep=""),paste("lamb",1:4,sep=""))
  Item1
})
names(ParList) <- paste("item",1:NUMBI,sep="")
perp1 <- rnorm(1000,0,1)
simdat1 <- NRM.sim(ParList,perp1)


reshdat <- reshMG(simdat1,items=1:5,correct=rep(1,5))

# hier mache ich eine neue Qmatrix im sinne von bock 1972

reshdat$Qmat

Qtest <- matrix(1/4,3,4)
diag(Qtest[,-1]) <- -0.75

QtestT <- t(Qtest)

Qtestlist <- lapply(1:10,function(x) QtestT)


newQmat <- mcIRT:::diagblock(Qtestlist)

## --------------------------------

reshdat$Qmat <- newQmat


#loadALL("/home/manuel/Dokumente/Manuel/packages/mcIRT/R")

ergSE1 <- nrm(reshdat)

ergSE1$SE


# konventionell

reshdat2 <- reshMG(simdat1,items=1:5,correct=rep(1,5))

ergSE2 <- nrm(reshdat2)

ergSE2$SE


ergSE1$ZLpar
ergSE2$ZLpar


################################ creating an other parametrization



NUMBI <- 5

ParList <- lapply(1:NUMBI,function(x)
{
  Item1 <- c(c(-2,-1,1,2),c(-1.2,0.3,0.2,0.7))
  names(Item1) <- c(paste("zeta",1:4,sep=""),paste("lamb",1:4,sep=""))
  Item1
})
names(ParList) <- paste("item",1:NUMBI,sep="")
perp1 <- rnorm(1000,0,1)
simdat1 <- NRM.sim(ParList,perp1)


reshdat <- reshMG(simdat1,items=1:5,correct=rep(1,5),paraM="01")

zzz1 <- nrm(reshdat)

# 
da <- simdat1
items=1:5
correct=rep(1,5)
groups = NA








####### 2 GROUPS #######################

NUMBI <- 5

ParList <- lapply(1:NUMBI,function(x)
{
  Item1 <- c(c(-2,-1,1,2),c(-1.2,0.3,0.2,0.7))
  names(Item1) <- c(paste("zeta",1:4,sep=""),paste("lamb",1:4,sep=""))
  Item1
})


names(ParList) <- paste("item",1:NUMBI,sep="")
# Add names to each list element, because this names are added to the columns in the simulated data.frame.



perp1 <- rnorm(5000,0,1)
perp2 <- rnorm(5000,1,1)

simdat1 <- NRM.sim(ParList,perp1)
simdat2 <- NRM.sim(ParList,perp2)

simdat1 <- data.frame(ID=1:5000,simdat1)
simdat2 <- data.frame(ID=5001:10000,simdat2)

simdatalla <- merge(simdat1,simdat2,all=T)
simdatall  <- simdatalla[,-1]

head(simdatall)
gruAB <- factor(rep(c("A","B"),each=5000))

DAT1 <- data.frame(simdatall,ABgroup = gruAB)

head(DAT1)

# hier wird alles mit den 2 gruppen als "nodif" geschätzt
reshOBJ <- reshMG(DAT1,items=1:NUMBI,groups=NUMBI+1,correct=rep(1,NUMBI))


# da <- DAT1
# items=1:5
# groups=6
# correct=rep(1,5)
# correct=rep(1,NUMBI)
# 
design <- designTemp(ngru=2,nit=5,TYPE="NRM")

design[[1]][2,1:3] <- 2
design[[2]][2,3:5] <- 2

design <- designTemp(ngru=2,nit=5,TYPE="NLM")


design[[1]][2,c(1,3)] <- 2
design[[2]][2,c(2,5)] <- 2
design[[3]][2,3:5] <- 2
design[[4]][2,1:3] <- 2


#############################################################################################################
############################## NELM ------------------------------------------------------------------------
#############################################################################################################




Item1 <- c(1,-2,c(-0.5,0.3,0.2),c(-0.5,-0.3,0.8))
names(Item1) <- c("a","b",paste("zeta1",1:3,sep=""),paste("lamb",1:3,sep=""))

Item2 <- c(1,-1,c(-0.5,-0.3,0.8),c(-0.5,0.3,0.2))
names(Item2) <- c("a","b",paste("zeta1",1:3,sep=""),paste("lamb",1:3,sep=""))

Item3 <- c(1,0,c(-0.5,-0.3,0.8),c(-0.5,0.3,0.2))
names(Item3) <- c("a","b",paste("zeta1",1:3,sep=""),paste("lamb",1:3,sep=""))

Item4 <- c(1,1,c(-0.5,-0.3,0.8),c(-0.5,0.3,0.2))
names(Item4) <- c("a","b",paste("zeta1",1:3,sep=""),paste("lamb",1:3,sep=""))

Item5 <- c(1,2,c(-0.5,-0.3,0.8),c(-0.5,0.3,0.2))
names(Item5) <- c("a","b",paste("zeta1",1:3,sep=""),paste("lamb",1:3,sep=""))

ParList <- list(Item1=Item1,Item2=Item2,Item3=Item3,Item4=Item4,Item5=Item5)


names(ParList) <- paste("item",1:5,sep="")
# Add names to each list element, because this names are added to the columns in the simulated data.frame.



perp1 <- rnorm(5000,0,1)
perp2 <- rnorm(5000,1,1)

simdat1 <- NLM.sim(ParList,perp1)
simdat2 <- NLM.sim(ParList,perp2)

simdat1 <- data.frame(ID=1:5000,simdat1)
simdat2 <- data.frame(ID=5001:10000,simdat2)

simdatalla <- merge(simdat1,simdat2,all=T)
simdatall  <- simdatalla[,-1]

head(simdatall)
gruAB <- factor(rep(c("A","B"),each=5000))

DAT1 <- data.frame(simdatall,ABgroup = gruAB)

head(DAT1)

des1 <- designTemp(ngru=2,nit=5,TYPE="NLM")

des1[[3]][2,2:3] <- 2

reshdat <- reshMG(DAT1,items=1:5,groups=6,correct=rep(1,5),TYPE="NLM",echo=FALSE,design=des1)
ergnlm1 <- nelm(reshdat)

summary(ergnlm1)


da <- DAT1

reshdatX <- reshMG(simdat1,items=1:5,correct=rep(1,5),TYPE="NLM",echo=FALSE,paraM="01")
ergnlm1X <- nelm(reshdatX)

summary(ergnlm1X)
print(ergnlm1X)





