########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## SAOM analysis - Multi-group results (4)
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: January 13th, 2021
########################################################################################################################

# R PACKAGES REQUIRED
library(RSiena);library(ggplot2);library(ggpubr)

# DATA LOADING
rm(list=ls())
load('SAOM_results.RData')

########################################################################################################################

# All units put together
altogether <- sienaGroupCreate(sienaobjs)
print01Report(altogether)

########################################################################################################################

# RSIENA EFFECTS
groupeff <- getEffects(altogether)

# Structural effects
groupeff <- includeEffects(groupeff,gwespFF,outActSqrt,inPopSqrt,name='friendship')
groupeff <- includeInteraction(groupeff,recip,gwespFF,parameter=69)
# Controls
groupeff <- includeEffects(groupeff,altX,egoX,simX,interaction1='age')
groupeff <- includeEffects(groupeff,altX,egoX,simX,interaction1='tenure')
groupeff <- includeEffects(groupeff,altX,egoX,simX,interaction1='workhours')
groupeff <- includeEffects(groupeff,X,interaction1='sameteam')
groupeff <- includeEffects(groupeff,X,interaction1='comm_freq')

groupeff1 <- groupeff2 <- groupeff3 <- groupeff

# Model 1
groupeff1 <- includeEffects(groupeff1,X,interaction1='pos_gossipA')
groupeff1 <- includeEffects(groupeff1,X,interaction1='negmix_gossipA')

# model 2
groupeff2 <- includeEffects(groupeff2,X,interaction1='pos_gossipS')
groupeff2 <- includeEffects(groupeff2,X,interaction1='negmix_gossipS')  
groupeff2 <- includeEffects(groupeff2,X,interaction1='pos_gossipC')
groupeff2 <- includeEffects(groupeff2,X,interaction1='negmix_gossipC')
groupeff2 <- includeEffects(groupeff2,X,interaction1='inc_gossip')

# model 3
groupeff3 <- includeEffects(groupeff3,X,interaction1='pos_gossipA')
groupeff3 <- includeEffects(groupeff3,X,interaction1='negmix_gossipA')  
groupeff3 <- includeEffects(groupeff3,egoX,interaction1='targets_pos')
groupeff3 <- includeEffects(groupeff3,egoX,interaction1='targets_neg')
groupeff3 <- includeInteraction(groupeff3,egoX,X,interaction1=c('targets_pos','pos_gossipA'))
groupeff3 <- includeInteraction(groupeff3,egoX,X,interaction1=c('targets_neg','negmix_gossipA'))

########################################################################################################################

# RSIENA ALGORITHM
algorithm_all1 <- sienaAlgorithmCreate(projname='results_all_model1',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm_all2 <- sienaAlgorithmCreate(projname='results_all_model2',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm_all3 <- sienaAlgorithmCreate(projname='results_all_model3',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)

########################################################################################################################

# RSIENA RESULTS
siena_model_all1 <- siena07RunToConvergence(dat=altogether,eff=groupeff1,alg=algorithm_all1,
                                            ans0=NULL,modelName='model1.ans',batch=FALSE,verbose=FALSE,
                                            useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

siena_model_all2 <- siena07RunToConvergence(dat=altogether,eff=groupeff2,alg=algorithm_all2,
                                            ans0=NULL,modelName='model1.ans',batch=FALSE,verbose=FALSE,
                                            useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

siena_model_all3 <- siena07RunToConvergence(dat=altogether,eff=groupeff3,alg=algorithm_all3,
                                            ans0=NULL,modelName='model1.ans',batch=FALSE,verbose=FALSE,
                                            useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

########################################################################################################################

# PRESENTATION OF RESULTS
multigroup_results1 <- data.frame(effect=siena_model_all1$effects$effectName,
                                  theta=round(siena_model_all1$theta,2),SE_A=round(siena_model_all1$se,2),
                                  p=round(2*(1-pnorm(abs(siena_model_all1$theta)/siena_model_all1$se)),3),
                                  s=sig(2*(1-pnorm(abs(siena_model_all1$theta)/siena_model_all1$se))))

multigroup_results2 <- data.frame(effect=siena_model_all2$effects$effectName,
                                  theta=round(siena_model_all2$theta,2),SE_A=round(siena_model_all2$se,2),
                                  p=round(2*(1-pnorm(abs(siena_model_all2$theta)/siena_model_all2$se)),3),
                                  s=sig(2*(1-pnorm(abs(siena_model_all2$theta)/siena_model_all2$se))))

multigroup_results3 <- data.frame(effect=siena_model_all3$effects$effectName,
                                  theta=round(siena_model_all3$theta,2),SE_A=round(siena_model_all3$se,2),
                                  p=round(2*(1-pnorm(abs(siena_model_all3$theta)/siena_model_all3$se)),3),
                                  s=sig(2*(1-pnorm(abs(siena_model_all3$theta)/siena_model_all3$se))))

# Results
write.table(multigroup_results1[c(1:6,22,8,7,13:21,12,11,9:10),],
            'multigroup_results1.csv',sep=',',row.names=FALSE)
write.table(multigroup_results2[c(1:6,25,8,7,16:24,15,14,9:13),],
            'multigroup_results2.csv',sep=',',row.names=FALSE)
write.table(multigroup_results3[c(1:6,24,8,7,13:21,12,11,9:10,22:23,25:26),],
            'multigroup_results3.csv',sep=',',row.names=FALSE)

########################################################################################################################

# Check homogeneity
timetest.1 <- sienaTimeTest(siena_model_all1)
het_results1 <- as.data.frame(summary(timetest.1)[[3]])
het_results1$sig <- sig(het_results1$`p-value`)

timetest.2 <- sienaTimeTest(siena_model_all2)
het_results2 <- as.data.frame(summary(timetest.2)[[3]])
het_results2$sig <- sig(het_results2$`p-value`)

timetest.3 <- sienaTimeTest(siena_model_all3)
het_results3 <- as.data.frame(summary(timetest.3)[[3]])
het_results3$sig <- sig(het_results3$`p-value`)

write.table(het_results1[c(1:3,19,5,4,10:18,9,8,6,7),],
            'het_results1.csv',sep=',',row.names=TRUE)
write.table(het_results2[c(1:3,22,5,4,13:21,12,11,6:10),],
            'het_results2.csv',sep=',',row.names=TRUE)
write.table(het_results3[c(1:3,21,5,4,10:18,9,8,6,7,19:20,22:23),],
            'het_results3.csv',sep=',',row.names=TRUE)

########################################################################################################################

# GOODNESS OF FIT
allunits_GOF1 <- siena07(algorithm_GOF1,data=altogether,effects=groupeff1,
                         batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=siena_model_all1)
allunits_GOF2 <- siena07(algorithm_GOF2,data=altogether,effects=groupeff2,
                         batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=siena_model_all2)
allunits_GOF3 <- siena07(algorithm_GOF3,data=altogether,effects=groupeff3,
                         batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=siena_model_all3)

allunits_modelGOF <- vector('list',length=3)
names(allunits_modelGOF) <- c('Model 1','Model 2','Model 3')
for(i in seq_along(allunits_modelGOF)){
  allunits_modelGOF[[i]] <- vector('list',length=4)
  names(allunits_modelGOF[[i]]) <- c('outdegree','indegree','triadic census','geodesic distance')
}

for(i in seq_along(allunits_modelGOF)){ 
  allunits_modelGOF[[i]][['outdegree']] <- sienaGOF(allunits_sienaGOF[[i]],OutdegreeDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE)
  allunits_modelGOF[[i]][['indegree']] <- sienaGOF(allunits_sienaGOF[[i]],IndegreeDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE) 
  allunits_modelGOF[[i]][['triadic census']] <- sienaGOF(allunits_sienaGOF[[i]],TriadCensus,verbose=TRUE,varName='friendship')
  allunits_modelGOF[[i]][['geodesic distance']] <- sienaGOF(allunits_sienaGOF[[i]],GeodesicDistribution,verbose=TRUE,varName='friendship',cumulative=FALSE)
}

allunits_sienaGOF <- list(allunits_GOF1,allunits_GOF2,allunits_GOF3)

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
save.image('SAOM_multigroup.RData')
