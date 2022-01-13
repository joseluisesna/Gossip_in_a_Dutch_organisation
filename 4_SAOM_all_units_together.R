########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## SAOM analysis - combined networks (4)
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: December 30th, 2021
########################################################################################################################

# R PACKAGES REQUIRED
library(RSiena);library(ggplot2);library(ggpubr)

# DATA LOADING
rm(list=ls())
load('SAOM_results.RData')

########################################################################################################################

# Composition change file 
comp.change <- append(append(comp.change$A,comp.change$B),comp.change$C)

########################################################################################################################

# Order covariates by setting
covariates2 <- covariates2[order(covariates2$setting),]

########################################################################################################################

# Large matrix for all three units
largemtx <- matrix(data=10,nrow=nrow(covariates2),ncol=nrow(covariates2),dimnames=list(covariates2$ID,covariates2$ID))

fri1 <- fri2 <- posgosA <- neggosA <- posgosS <- neggosS <- posgosC <- neggosC <- gosInc <- commfreq <- sameteam <-  largemtx

# Friendship time 1
fri1[1:47,1:47] <- ntw$friendship$AW1
fri1[48:110,48:110] <- ntw$friendship$BW1
fri1[111:156,111:156] <- ntw$friendship$CW1
# Friendship time 2
fri2[1:47,1:47] <- ntw$friendship_imp$AW2
fri2[48:110,48:110] <- ntw$friendship_imp$BW2
fri2[111:156,111:156] <- ntw$friendship_imp$CW2
# Positive gossip (all)
posgosA[1:47,1:47] <- gossipApos$Ap
posgosA[48:110,48:110] <- gossipApos$Bp
posgosA[111:156,111:156] <- gossipApos$Cp
# Negative gossip (all)
neggosA[1:47,1:47] <- gossipAnegmix$Anm
neggosA[48:110,48:110] <- gossipAnegmix$Bnm
neggosA[111:156,111:156] <- gossipAnegmix$Cnm
# Positive gossip (one sender only)
posgosS[1:47,1:47] <- gossipSpos$Ap
posgosS[48:110,48:110] <- gossipSpos$Bp
posgosS[111:156,111:156] <- gossipSpos$Cp
# Negative gossip (one sender only)
neggosS[1:47,1:47] <- gossipSnegmix$Anm
neggosS[48:110,48:110] <- gossipSnegmix$Bnm
neggosS[111:156,111:156] <- gossipSnegmix$Cnm
# Positive gossip (several senders)
posgosC[1:47,1:47] <- gossipCpos$Ap
posgosC[48:110,48:110] <- gossipCpos$Bp
posgosC[111:156,111:156] <- gossipCpos$Cp
# Negative gossip (several senders)
neggosC[1:47,1:47] <- gossipCnegmix$Anm
neggosC[48:110,48:110] <- gossipCnegmix$Bnm
neggosC[111:156,111:156] <- gossipCnegmix$Cnm
# Incongruent gossip
gosInc[1:47,1:47] <- gossipI$A
gosInc[48:110,48:110] <- gossipI$B
gosInc[111:156,111:156] <- gossipI$C
# Communication frequency
commfreq[1:47,1:47] <- ntw$communication$A
commfreq[48:110,48:110] <- ntw$communication$B
commfreq[111:156,111:156] <- ntw$communication$C
# Same team
sameteam[1:47,1:47] <- 1*outer(covs$A$team,covs$A$team,'==')
sameteam[48:110,48:110] <- 1*outer(covs$B$team,covs$B$team,'==')
sameteam[111:156,111:156] <- 1*outer(covs$C$team,covs$C$team,'==')

########################################################################################################################

# SIENA OBJECT CREATION
siena_obj <- sienaDataCreate(
  friendship = sienaNet(array(c(fri1,fri2),dim=c(nrow(fri1),ncol(fri1),2))),
  # all gossip
  pos_gossipA = coDyadCovar(posgosA),
  negmix_gossipA = coDyadCovar(neggosA),
  # simple gossip
  pos_gossipS = coDyadCovar(posgosS),
  negmix_gossipS = coDyadCovar(neggosS),
  # complex gossip
  pos_gossipC = coDyadCovar(posgosC),
  negmix_gossipC = coDyadCovar(neggosC),
  # incongruent gossip (from different sourcers)
  inc_gossip = coDyadCovar(gosInc),
  # communication frequency
  comm_freq = coDyadCovar(commfreq),
  # covariates
  age = coCovar(covariates2$age,centered=TRUE), 
  tenure = coCovar(covariates2$tenure,centered=TRUE),
  workhours = coCovar(covariates2$workhours,centered=TRUE),
  sameteam = coDyadCovar(sameteam), # being part of the same work team
  # Number of gossip targets
  targets_pos = coCovar(covariates2$pos_targets,centered=TRUE),
  targets_neg = coCovar(covariates2$neg_targets,centered=TRUE),
  # Composition change file
  sienaCompositionChange(comp.change)
) 

########################################################################################################################

# RSIENA EFFECTS
# Structural effects: outdegree, reciprocity, GWESP, outdegree activity, indegree popularity, reciprocity*GWESP
sienaeff <- getEffects(siena_obj)
sienaeff <- includeEffects(sienaeff,gwespFF,outActSqrt,inPopSqrt,name='friendship')
sienaeff <- includeInteraction(sienaeff,recip,gwespFF,parameter=69)
sienaeff <- includeEffects(sienaeff,altX,egoX,simX,interaction1='age')
sienaeff <- includeEffects(sienaeff,altX,egoX,simX,interaction1='tenure')
sienaeff <- includeEffects(sienaeff,altX,egoX,simX,interaction1='workhours')
sienaeff <- includeEffects(sienaeff,X,interaction1='sameteam')
sienaeff <- includeEffects(sienaeff,X,interaction1='comm_freq')

# Effects for models 1, 2, and 3
sienaeffm1 <- sienaeffm2 <- sienaeffm3 <- sienaeff

# Model 1 (positive and negative gossip)
sienaeffm1 <- includeEffects(sienaeffm1,X,interaction1='pos_gossipA')
sienaeffm1 <- includeEffects(sienaeffm1,X,interaction1='negmix_gossipA')
# Model 2 (simple vs complex gossip, plus incongruent gossip)
sienaeffm2 <- includeEffects(sienaeffm2,X,interaction1='pos_gossipS')
sienaeffm2 <- includeEffects(sienaeffm2,X,interaction1='negmix_gossipS')  
sienaeffm2 <- includeEffects(sienaeffm2,X,interaction1='pos_gossipC')
sienaeffm2 <- includeEffects(sienaeffm2,X,interaction1='negmix_gossipC')
sienaeffm2 <- includeEffects(sienaeffm2,X,interaction1='inc_gossip')
# Model 3 (positive and negative gossip, plus interactions with number of targets)
sienaeffm3 <- includeEffects(sienaeffm3,X,interaction1='pos_gossipA')
sienaeffm3 <- includeEffects(sienaeffm3,X,interaction1='negmix_gossipA')  
sienaeffm3 <- includeEffects(sienaeffm3,egoX,interaction1='targets_pos')
sienaeffm3 <- includeEffects(sienaeffm3,egoX,interaction1='targets_neg')
sienaeffm3 <- includeInteraction(sienaeffm3,egoX,X,interaction1=c('targets_pos','pos_gossipA'))
sienaeffm3 <- includeInteraction(sienaeffm3,egoX,X,interaction1=c('targets_neg','negmix_gossipA'))

########################################################################################################################

# RSIENA RESULTS
allunits1 <- siena07RunToConvergence(dat=siena_obj,eff=sienaeffm1,alg=algorithm1,
                                     ans0=NULL,modelName='model1.ans',batch=FALSE,verbose=FALSE,
                                     useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

allunits2 <- siena07RunToConvergence(dat=siena_obj,eff=sienaeffm2,alg=algorithm2,
                                     ans0=NULL,modelName='model2.ans',batch=FALSE,verbose=FALSE,
                                     useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

allunits3 <- siena07RunToConvergence(dat=siena_obj,eff=sienaeffm3,alg=algorithm3,
                                     ans0=NULL,modelName='model3.ans',batch=FALSE,verbose=FALSE,
                                     useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

########################################################################################################################

# PRESENTATION OF RESULTS
allunits_results1 <- data.frame(effect=allunits1$effects$effectName,
                                theta=round(allunits1$theta,2),SE_A=round(allunits1$se,2),
                                p=round(2*(1-pnorm(abs(allunits1$theta)/allunits1$se)),3),
                                s=sig(2*(1-pnorm(abs(allunits1$theta)/allunits1$se))))
allunits_results2 <- data.frame(effect=allunits2$effects$effectName,
                                theta=round(allunits2$theta,2),SE_A=round(allunits2$se,2),
                                p=round(2*(1-pnorm(abs(allunits2$theta)/allunits2$se)),3),
                                s=sig(2*(1-pnorm(abs(allunits2$theta)/allunits2$se))))
allunits_results3 <- data.frame(effect=allunits3$effects$effectName,
                                theta=round(allunits3$theta,2),SE_A=round(allunits3$se,2),
                                p=round(2*(1-pnorm(abs(allunits3$theta)/allunits3$se)),3),
                                s=sig(2*(1-pnorm(abs(allunits3$theta)/allunits3$se))))

# Results
write.table(allunits_results1,'allunits_results1.csv',sep=',',row.names=FALSE)
write.table(allunits_results2,'allunits_results2.csv',sep=',',row.names=FALSE)
write.table(allunits_results3,'allunits_results3.csv',sep=',',row.names=FALSE)

########################################################################################################################

# GOODNESS OF FIT
allunits_GOF1 <- siena07(algorithm_GOF1,data=siena_obj,effects=sienaeffm1,
                         batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=allunits1)
allunits_GOF2 <- siena07(algorithm_GOF2,data=siena_obj,effects=sienaeffm2,
                         batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=allunits2)
allunits_GOF3 <- siena07(algorithm_GOF3,data=siena_obj,effects=sienaeffm3,
                         batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=allunits3)

allunits_modelGOF <- vector('list',length=3)
names(allunits_modelGOF) <- c('Model 1','Model 2','Model 3')
for(i in seq_along(allunits_modelGOF)){
  allunits_modelGOF[[i]] <- vector('list',length=4)
  names(allunits_modelGOF[[i]]) <- c('outdegree','indegree','triadic census','geodesic distance')
}

allunits_sienaGOF <- list(allunits_GOF1,allunits_GOF2,allunits_GOF3)

# For some reason, GOF does not work for model 2
for(i in c(1,3)){ 
  allunits_modelGOF[[i]][['outdegree']] <- sienaGOF(allunits_sienaGOF[[i]],OutdegreeDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE)
  allunits_modelGOF[[i]][['indegree']] <- sienaGOF(allunits_sienaGOF[[i]],IndegreeDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE) 
  allunits_modelGOF[[i]][['triadic census']] <- sienaGOF(allunits_sienaGOF[[i]],TriadCensus,verbose=TRUE,varName='friendship')
  allunits_modelGOF[[i]][['geodesic distance']] <- sienaGOF(allunits_sienaGOF[[i]],GeodesicDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE)
}

allunits_modelGOF <- allunits_modelGOF[c('Model 1','Model 3')]

########################################################################################################################

# VISUALISATION
for(i in seq_along(allunits_modelGOF)){
  for(j in seq_along(allunits_modelGOF[[i]])){
    allunits_modelGOF[[i]][[j]] <- plot(allunits_modelGOF[[i]][[j]],scale=TRUE,center=TRUE)
  }
}

# Model 1
jpeg(filename='GOFmodel1.jpeg',width=24,height=16,units='in',res=500)
ggarrange(modelGOF[[1]][['Unit A']][[1]],modelGOF[[1]][['Unit A']][[2]],modelGOF[[1]][['Unit A']][[3]],modelGOF[[1]][['Unit A']][[4]],
          modelGOF[[1]][['Unit B']][[1]],modelGOF[[1]][['Unit B']][[2]],modelGOF[[1]][['Unit B']][[3]],modelGOF[[1]][['Unit B']][[4]],
          modelGOF[[1]][['Unit C']][[1]],modelGOF[[1]][['Unit C']][[2]],modelGOF[[1]][['Unit C']][[3]],modelGOF[[1]][['Unit C']][[4]],
          allunits_modelGOF[[1]][[1]],allunits_modelGOF[[1]][[2]],allunits_modelGOF[[1]][[3]],allunits_modelGOF[[1]][[4]],
          labels=c('A','','','','B','','','','C','','','','All','','',''),
          ncol=4,nrow=4)
dev.off()
# Model 2
jpeg(filename='GOFmodel2.jpeg',width=24,height=12,units='in',res=500)
ggarrange(modelGOF[[2]][['Unit A']][[1]],modelGOF[[2]][['Unit A']][[2]],modelGOF[[2]][['Unit A']][[3]],modelGOF[[2]][['Unit A']][[4]],
          modelGOF[[2]][['Unit B']][[1]],modelGOF[[2]][['Unit B']][[2]],modelGOF[[2]][['Unit B']][[3]],modelGOF[[2]][['Unit B']][[4]],
          modelGOF[[2]][['Unit C']][[1]],modelGOF[[2]][['Unit C']][[2]],modelGOF[[2]][['Unit C']][[3]],modelGOF[[2]][['Unit C']][[4]],
          labels=c('A','','','','B','','','','C','','',''),
          ncol=4,nrow=3)
dev.off()
# Model 3
jpeg(filename='GOFmodel3.jpeg',width=24,height=16,units='in',res=500)
ggarrange(modelGOF[[3]][['Unit A']][[1]],modelGOF[[3]][['Unit A']][[2]],modelGOF[[3]][['Unit A']][[3]],modelGOF[[3]][['Unit A']][[4]],
          modelGOF[[3]][['Unit B']][[1]],modelGOF[[3]][['Unit B']][[2]],modelGOF[[3]][['Unit B']][[3]],modelGOF[[3]][['Unit B']][[4]],
          modelGOF[[3]][['Unit C']][[1]],modelGOF[[3]][['Unit C']][[2]],modelGOF[[3]][['Unit C']][[3]],modelGOF[[3]][['Unit C']][[4]],
          allunits_modelGOF[[2]][[1]],allunits_modelGOF[[2]][[2]],allunits_modelGOF[[2]][[3]],allunits_modelGOF[[2]][[4]],
          labels=c('A','','','','B','','','','C','','','','All','','',''),
          ncol=4,nrow=4)
dev.off()

########################################################################################################################

# Save image
save.image('SAOM_allunits.RData')