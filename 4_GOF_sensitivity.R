########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## SAOM Sensitivity check and Goodness of fit (4)
## R script written by Jose Luis Estevez (Linkoping University)
## Date: October 28th, 2020
########################################################################################################################

# R PACKAGES REQUIRED
library(tidyverse);library(RSiena)

########################################################################################################################

# DATA LOADING
rm(list=ls())
load('siena_results.RData')

########################################################################################################################

# SAOM GOODNESS OF FIT 

# Only cut-off 20
sienaObj <- sienaObj[c(3,9,15)]
for(i in seq_along(siena_models)){
    siena_models[[i]] <- siena_models[[i]][c(3,9,15)]
    siena_effects[[i]] <- siena_effects[[i]][c(3,9,15)]
}

# Goodness of fit
siena_GOF <- vector('list',3)
algorithm_GOF <- sienaAlgorithmCreate(projname='gof',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 

for(x in seq_along(siena_models)){
  for(i in seq_along(siena_models[[x]])){
    siena_GOF[[x]][[i]] <- siena07(algorithm_GOF,data=sienaObj[[i]],effects=siena_effects[[x]][[i]],
                                   batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,
                                   prevAns=siena_models[[x]][[i]])
  }
}

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

########################################################################################################################

GOF_sum <- vector('list',3)
for(i in seq_along(GOF_sum)){
  GOF_sum[[i]] <- vector('list',3)
}

for(x in seq_along(siena_GOF)){
  for(i in seq_along(siena_GOF[[x]])){
      GOF_sum[[x]][[i]][['inD']] <- sienaGOF(siena_GOF[[x]][[i]],IndegreeDistribution,verbose=TRUE,
                                             varName='friendship',cumulative=FALSE)
      GOF_sum[[x]][[i]][['outD']] <- sienaGOF(siena_GOF[[x]][[i]],OutdegreeDistribution,verbose=TRUE,
                                             varName='friendship',cumulative=FALSE)
      GOF_sum[[x]][[i]][['triC']] <- sienaGOF(siena_GOF[[x]][[i]],TriadCensus,verbose=TRUE,varName='friendship')
      GOF_sum[[x]][[i]][['geoD']] <- sienaGOF(siena_GOF[[x]][[i]],GeodesicDistribution,verbose=TRUE,
                                              varName='friendship',cumulative=FALSE)
  }
}

# Visualisation
GOF_vis <- vector('list',3)
for(i in seq_along(GOF_vis)){
  GOF_vis[[i]] <- vector('list',3)
}

for(x in seq_along(GOF_sum)){
  for(i in seq_along(GOF_sum[[x]])){
    for(stat in names(GOF_sum[[x]][[i]])){
      GOF_vis[[x]][[i]][[stat]] <- plot(GOF_sum[[x]][[i]][[stat]],scale=TRUE,center=TRUE)
    }
  }
}