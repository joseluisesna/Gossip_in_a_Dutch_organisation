########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## SAOM analysis (3)
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: May 29th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(RSiena);library(ggplot2);library(ggpubr)

# DATA LOADING
rm(list=ls())
load('sienadata.RData')

########################################################################################################################

# COMPOSITION-CHANGE FILE FOR JOINERS AND LEAVERS
comp.change <- list()
comp.change[[1]] <- vector('list',length=length(unitA))
comp.change[[2]] <- vector('list',length=length(unitB))
comp.change[[3]] <- vector('list',length=length(unitC))

names(comp.change[[1]]) <- unitA
names(comp.change[[2]]) <- unitB
names(comp.change[[3]]) <- unitC
names(comp.change) <- c('A','B','C')

set.seed(0708)
for(i in seq_along(unitA)){
  if(unitA[i] %in% unitA1){
    comp.change[['A']][[i]][1] <- 1}else{comp.change[['A']][[i]][1] <- runif(1,min=1.01,max=1.99)}
  if(unitA[i] %in% unitA2){
    comp.change[['A']][[i]][2] <- 2}else{comp.change[['A']][[i]][2] <- runif(1,min=1.01,max=1.99)}
}

for(i in seq_along(unitB)){
  if(unitB[i] %in% unitB1){
    comp.change[['B']][[i]][1] <- 1}else{comp.change[['B']][[i]][1] <- runif(1,min=1.01,max=1.99)}
  if(unitB[i] %in% unitB2){
    comp.change[['B']][[i]][2] <- 2}else{comp.change[['B']][[i]][2] <- runif(1,min=1.01,max=1.99)}
}

for(i in seq_along(unitC)){
  if(unitC[i] %in% unitC1){
    comp.change[['C']][[i]][1] <- 1}else{comp.change[['C']][[i]][1] <- runif(1,min=1.01,max=1.99)}
  if(unitC[i] %in% unitC2){
    comp.change[['C']][[i]][2] <- 2}else{comp.change[['C']][[i]][2] <- runif(1,min=1.01,max=1.99)}
}

########################################################################################################################

# RSIENA: OBJECT CREATION
# Divide covariates by unit
covs <- list()
covs[['A']] <- covariates2[covariates2$setting == 'Unit A',]
covs[['B']] <- covariates2[covariates2$setting == 'Unit B',]
covs[['C']] <- covariates2[covariates2$setting == 'Unit C',]

# All gossip (By tone)
gossipApos <- gossipA[c('Ap','Bp','Cp')]
gossipAneg <- gossipA[c('An','Bn','Cn')]
gossipAnegmix <- gossipA[c('Anm','Bnm','Cnm')]
# Simple gossip
gossipSpos <- gossipS[c('Ap','Bp','Cp')]
gossipSneg <- gossipS[c('An','Bn','Cn')]
gossipSnegmix <- gossipS[c('Anm','Bnm','Cnm')]
# Complex gossip
gossipCpos <- gossipC[c('Ap','Bp','Cp')]
gossipCneg <- gossipC[c('An','Bn','Cn')]
gossipCnegmix <- gossipC[c('Anm','Bnm','Cnm')]

sienaobjs <- vector('list',3)
names(sienaobjs) <- c('A','B','C')

for(i in seq_along(sienaobjs)){
  sienaobjs[[i]] <- sienaDataCreate(
    friendship = sienaNet(array(c(ntw$friendship[[i]],ntw$friendship_imp[[i]]),
                                dim=c(nrow(ntw$friendship[[i]]),ncol(ntw$friendship[[i]]),2))),
    # all gossip
    pos_gossipA = coDyadCovar(gossipApos[[i]]),
    neg_gossipA = coDyadCovar(gossipAneg[[i]]),
    negmix_gossipA = coDyadCovar(gossipAnegmix[[i]]),
    # simple gossip
    pos_gossipS = coDyadCovar(gossipSpos[[i]]),
    neg_gossipS = coDyadCovar(gossipSneg[[i]]),
    negmix_gossipS = coDyadCovar(gossipSnegmix[[i]]),
    # complex gossip
    pos_gossipC = coDyadCovar(gossipCpos[[i]]),
    neg_gossipC = coDyadCovar(gossipCneg[[i]]),
    negmix_gossipC = coDyadCovar(gossipCnegmix[[i]]),
    # incongruent gossip (from different sourcers)
    inc_gossip = coDyadCovar(gossipI[[i]]),
    # communication frequency
    comm_freq = coDyadCovar(ntw$communication[[i]]),
    # covariates
    age = coCovar(covs[[i]]$age,centered=TRUE), 
    tenure = coCovar(covs[[i]]$tenure,centered=TRUE),
    workhours = coCovar(covs[[i]]$workhours,centered=TRUE),
    sameteam = coDyadCovar(1*outer(covs[[i]]$team,covs[[i]]$team,'==')), # being part of the same work team
    # Number of gossip targets
    targets_pos = coCovar(covs[[i]]$pos_targets,centered=TRUE),
    targets_neg = coCovar(covs[[i]]$neg_targets,centered=TRUE),
    # Composition change file
    sienaCompositionChange(comp.change[[i]])
  )
}

########################################################################################################################

# RSIENA EFFECTS
sienaeff <- vector('list',3)
names(sienaeff) <- c('A','B','C')

for(i in seq_along(sienaeff)){
  # Structural effects: outdegree, reciprocity, GWESP, outdegree activity, indegree popularity, reciprocity*GWESP
  sienaeff[[i]] <- getEffects(sienaobjs[[i]])
  sienaeff[[i]] <- includeEffects(sienaeff[[i]],gwespFF,outActSqrt,inPopSqrt,name='friendship')
  sienaeff[[i]] <- includeInteraction(sienaeff[[i]],recip,gwespFF,parameter=69)
  # Controls
  sienaeff[[i]] <- includeEffects(sienaeff[[i]],altX,egoX,simX,interaction1='age')
  sienaeff[[i]] <- includeEffects(sienaeff[[i]],altX,egoX,simX,interaction1='tenure')
  sienaeff[[i]] <- includeEffects(sienaeff[[i]],altX,egoX,simX,interaction1='workhours')
  sienaeff[[i]] <- includeEffects(sienaeff[[i]],X,interaction1='sameteam')
  sienaeff[[i]] <- includeEffects(sienaeff[[i]],X,interaction1='comm_freq')
}

# Effects for models 1, 2, and 3
sienaeffm1 <- sienaeffm2 <- sienaeffm3 <- sienaeff

# Model 1 (positive and negative gossip)
for(i in seq_along(sienaeffm1)){
  sienaeffm1[[i]] <- includeEffects(sienaeffm1[[i]],X,interaction1='pos_gossipA')
  sienaeffm1[[i]] <- includeEffects(sienaeffm1[[i]],X,interaction1='negmix_gossipA')
}
# Model 2 (simple vs complex gossip, plus incongruent gossip)
for(i in seq_along(sienaeffm2)){
  sienaeffm2[[i]] <- includeEffects(sienaeffm2[[i]],X,interaction1='pos_gossipS')
  sienaeffm2[[i]] <- includeEffects(sienaeffm2[[i]],X,interaction1='negmix_gossipS')  
  sienaeffm2[[i]] <- includeEffects(sienaeffm2[[i]],X,interaction1='pos_gossipC')
  sienaeffm2[[i]] <- includeEffects(sienaeffm2[[i]],X,interaction1='negmix_gossipC')
  sienaeffm2[[i]] <- includeEffects(sienaeffm2[[i]],X,interaction1='inc_gossip')
}
# Model 3 (positive and negative gossip, plus interactions with number of targets)
for(i in seq_along(sienaeffm3)){
  sienaeffm3[[i]] <- includeEffects(sienaeffm3[[i]],X,interaction1='pos_gossipA')
  sienaeffm3[[i]] <- includeEffects(sienaeffm3[[i]],X,interaction1='negmix_gossipA')  
  sienaeffm3[[i]] <- includeEffects(sienaeffm3[[i]],egoX,interaction1='targets_pos')
  sienaeffm3[[i]] <- includeEffects(sienaeffm3[[i]],egoX,interaction1='targets_neg')
  sienaeffm3[[i]] <- includeInteraction(sienaeffm3[[i]],egoX,X,interaction1=c('targets_pos','pos_gossipA'))
  sienaeffm3[[i]] <- includeInteraction(sienaeffm3[[i]],egoX,X,interaction1=c('targets_neg','negmix_gossipA'))
}

########################################################################################################################

# RSIENA ALGORITHM
algorithm1 <- sienaAlgorithmCreate(projname='results1',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm2 <- sienaAlgorithmCreate(projname='results2',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm3 <- sienaAlgorithmCreate(projname='results3',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)

# Function for running siena07 until convergence has been reached
siena07RunToConvergence <- function(alg, data, eff, ans0, modelName, ...){
  numr <- 0
  ans <- siena07(alg, data = data, eff = eff, prevAns = ans0, returnDeps = TRUE, ...)
  
  repeat{
    numr <- numr + 1
    tconv.max <- ans$tconv.max
    tratio.max <- max( abs( ans$tstat[eff$type[eff$include] != "rate"] ) )
    if (tconv.max > 100) {
      print(ans)
      cat("WARNING: Extreme divergence. Terminating run.\n")
      return("WARNING: Extreme divergence. Terminating run")
    }
    else if (tconv.max < 0.20 & tratio.max < 0.10) {
      print(ans)
      cat(paste0("Maximum Absolute Value Amongst Convergence t-Ratios: ", tratio.max, "\n"))
      cat(paste0("Model Has Converged After ", numr, " iterations. \n"))
      return(ans)
    }
    else {
      print(ans)
      cat("WARNING: Convergence Inadequate.\n")
      cat(paste0("Overall maximum convergence ratio: ", tconv.max, "\n"))
      cat(paste0("Iteration Number: ", numr), "\n")
      ans <- siena07(alg, data = data, eff = eff, prevAns = ans, returnDeps = TRUE, ...)
    }
  }
}

########################################################################################################################

# RSIENA RESULTS
siena_models1 <- list()
siena_models2 <- list()
siena_models3 <- list()

for(i in seq_along(sienaobjs)){
  siena_models1[[i]] <- siena07RunToConvergence(dat=sienaobjs[[i]],eff=sienaeffm1[[i]],alg=algorithm1,
                                                ans0=NULL,modelName='model1.ans',batch=FALSE,verbose=FALSE,
                                                useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
}

for(i in seq_along(sienaobjs)){
  siena_models2[[i]] <- siena07RunToConvergence(dat=sienaobjs[[i]],eff=sienaeffm2[[i]],alg=algorithm2,
                                                ans0=NULL,modelName='model2.ans',batch=FALSE,verbose=FALSE,
                                                useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
}

for(i in seq_along(sienaobjs)){
  siena_models3[[i]] <- siena07RunToConvergence(dat=sienaobjs[[i]],eff=sienaeffm3[[i]],alg=algorithm3,
                                                ans0=NULL,modelName='model3.ans',batch=FALSE,verbose=FALSE,
                                                useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
}

########################################################################################################################

# PRESENTATION OF RESULTS
# Results as table
sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*','')))
}

outcome <- function(output){
  tab <- data.frame(effect=output[[1]]$effects$effectName,
                    theta_A=round(output[[1]]$theta,2),SE_A=round(output[[1]]$se,2),
                    p_A=round(2*(1-pnorm(abs(output[[1]]$theta)/output[[1]]$se)),3),
                    s_A=sig(2*(1-pnorm(abs(output[[1]]$theta)/output[[1]]$se))),
                    
                    theta_B=round(output[[2]]$theta,2),SE_B=round(output[[2]]$se,2),
                    p_B=round(2*(1-pnorm(abs(output[[2]]$theta)/output[[2]]$se)),3),
                    s_B=sig(2*(1-pnorm(abs(output[[2]]$theta)/output[[2]]$se))),
                    
                    theta_C=round(output[[3]]$theta,2),SE_C=round(output[[3]]$se,2),
                    p_C=round(2*(1-pnorm(abs(output[[3]]$theta)/output[[3]]$se)),3),
                    s_C=sig(2*(1-pnorm(abs(output[[3]]$theta)/output[[3]]$se))))
  return(tab)
}

# Results
write.table(outcome(siena_models1),'siena_models1.csv',sep=',',row.names=FALSE)
write.table(outcome(siena_models2),'siena_models2.csv',sep=',',row.names=FALSE)
write.table(outcome(siena_models3),'siena_models3.csv',sep=',',row.names=FALSE)

########################################################################################################################

# GOODNESS OF FIT
# Algorithms
algorithm_GOF1 <- sienaAlgorithmCreate(projname='GOF1',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 
algorithm_GOF2 <- sienaAlgorithmCreate(projname='GOF2',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 
algorithm_GOF3 <- sienaAlgorithmCreate(projname='GOF3',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 

sienaGOF1 <- sienaGOF2 <- sienaGOF3 <- list()

for(i in seq_along(siena_models1)){
  sienaGOF1[[i]] <- siena07(algorithm_GOF1,data=sienaobjs[[i]],effects=sienaeffm1[[i]],
                            batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=siena_models1[[i]])
}

for(i in seq_along(siena_models2)){
  sienaGOF2[[i]] <- siena07(algorithm_GOF2,data=sienaobjs[[i]],effects=sienaeffm2[[i]],
                            batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=siena_models2[[i]])
}

for(i in seq_along(siena_models3)){
  sienaGOF3[[i]] <- siena07(algorithm_GOF3,data=sienaobjs[[i]],effects=sienaeffm3[[i]],
                            batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=siena_models3[[i]])
}

########################################################################################################################

# RETRIEVAL OF GOODNESS OF FIT STATISTICS
# Function to obtain the geodesic distribution
igraphNetworkExtraction <- function(i,data,sims,period,groupName,varName){
  dimsOfDepVar<- attr(data[[groupName]]$depvars[[varName]], 'netdims')
  missings <- is.na(data[[groupName]]$depvars[[varName]][,,period]) |
    is.na(data[[groupName]]$depvars[[varName]][,,period+1])
  if (is.null(i)) {
    # sienaGOF wants the observation:
    original <- data[[groupName]]$depvars[[varName]][,,period+1]
    original[missings] <- 0
    returnValue <- graph.adjacency(original)
  } else {
    missings <- graph.adjacency(missings)
    #sienaGOF wants the i-th simulation:
    returnValue <- graph.difference(graph.edgelist(sims[[i]][[groupName]][[varName]][[period]][,1:2]),missings)
  }
  returnValue
}

GeodesicDistribution <- function(i,data,sims,period,groupName,varName,levls=c(1:5,Inf),cumulative=TRUE,...){
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  a <- sna::geodist(x)$gdist
  if(cumulative){
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  } else {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

# Object for results
modelGOF <- vector('list',length=3)
names(modelGOF) <- c('Model 1','Model 2','Model 3')
for(i in seq_along(modelGOF)){
  modelGOF[[i]] <- vector('list',length=3)
  names(modelGOF[[i]]) <- c('Unit A','Unit B','Unit C')
  for(j in seq_along(modelGOF[[i]])){
    modelGOF[[i]][[j]] <- vector('list',length=4)
    names(modelGOF[[i]][[j]]) <- c('outdegree','indegree','triadic census','geodesic distance')
  }
}

sienaGOF <- list(sienaGOF1,sienaGOF2,sienaGOF3)

for(i in seq_along(modelGOF)){
  for(j in seq_along(modelGOF[[i]])){
    modelGOF[[i]][[j]][['outdegree']] <- sienaGOF(sienaGOF[[i]][[j]],OutdegreeDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE)
    modelGOF[[i]][[j]][['indegree']] <- sienaGOF(sienaGOF[[i]][[j]],IndegreeDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE) 
    modelGOF[[i]][[j]][['triadic census']] <- sienaGOF(sienaGOF[[i]][[j]],TriadCensus,verbose=TRUE,varName='friendship')
    modelGOF[[i]][[j]][['geodesic distance']] <- sienaGOF(sienaGOF[[i]][[j]],GeodesicDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE)
  }
}

########################################################################################################################

# VISUALISATION
for(i in seq_along(modelGOF)){
  for(j in seq_along(modelGOF[[i]])){
    for(k in seq_along(modelGOF[[i]][[j]])){
      modelGOF[[i]][[j]][[k]] <- plot(modelGOF[[i]][[j]][[k]],scale=TRUE,center=TRUE)
    }
  }
}

# Model 1
jpeg(filename='GOFmodel1.jpeg',width=24,height=12,units='in',res=500)
ggarrange(modelGOF[[1]][['Unit A']][[1]],modelGOF[[1]][['Unit A']][[2]],modelGOF[[1]][['Unit A']][[3]],modelGOF[[1]][['Unit A']][[4]],
          modelGOF[[1]][['Unit B']][[1]],modelGOF[[1]][['Unit B']][[2]],modelGOF[[1]][['Unit B']][[3]],modelGOF[[1]][['Unit B']][[4]],
          modelGOF[[1]][['Unit C']][[1]],modelGOF[[1]][['Unit C']][[2]],modelGOF[[1]][['Unit C']][[3]],modelGOF[[1]][['Unit C']][[4]],
          labels=c('A','','','','B','','','','C','','',''),
          ncol=4,nrow=3)
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
jpeg(filename='GOFmodel3.jpeg',width=24,height=12,units='in',res=500)
ggarrange(modelGOF[[3]][['Unit A']][[1]],modelGOF[[3]][['Unit A']][[2]],modelGOF[[3]][['Unit A']][[3]],modelGOF[[3]][['Unit A']][[4]],
          modelGOF[[3]][['Unit B']][[1]],modelGOF[[3]][['Unit B']][[2]],modelGOF[[3]][['Unit B']][[3]],modelGOF[[3]][['Unit B']][[4]],
          modelGOF[[3]][['Unit C']][[1]],modelGOF[[3]][['Unit C']][[2]],modelGOF[[3]][['Unit C']][[3]],modelGOF[[3]][['Unit C']][[4]],
          labels=c('A','','','','B','','','','C','','',''),
          ncol=4,nrow=3)
dev.off()

########################################################################################################################

# Save image
save.image('SAOM_results.RData')