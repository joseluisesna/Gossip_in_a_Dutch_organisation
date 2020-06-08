########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## SAOM Sensitivity check and Goodness of fit
## R script written by J. Luis Estevez (University of Groningen)
## Date: June 8th, 2019
########################################################################################################################

# R PACKAGES REQUIRED
library(tidyverse)
library(RSiena)

########################################################################################################################

# DATA LOADING
load('siena_results.RData')

########################################################################################################################

# SENSITIVITY

# For models with convergence above .25, rerun
siena_models <- list(siena_models1,siena_models2,siena_models3)
siena_effects <- list(siena_effects1,siena_effects2,siena_effects3)
rm(siena_models1);rm(siena_models2);rm(siena_models3);rm(siena_effects1);rm(siena_effects2);rm(siena_effects3)

algorithm4 <- sienaAlgorithmCreate(projname='results4',useStdInits=FALSE,cond=FALSE,nsub=1,n3=5000,seed=0708)

# Repeat the following command thrice for all models to below below .25
for(x in seq_along(siena_models)){
  for(i in seq_along(siena_models[[x]])){
    if(siena_models[[x]][[i]]$tconv.max >= .25){
      siena_models[[x]][[i]] <- siena07(algorithm4,data=sienaObj[[i]],effects=siena_effects[[x]][[i]],
                                        batch=FALSE,verbose=FALSE,useCluster=TRUE,nbrNodes=3,returnChains=FALSE,
                                        returnDataFrame=TRUE,prevAns=siena_models[[x]][[i]])
    }
  }
}

########################################################################################################################

# Extraction of SAOM information
sens <- vector('list',3)

for(x in seq_along(siena_models)){
  for(i in seq_along(siena_models[[x]])){
    sens[[x]][[i]] <- matrix(NA,nrow=length(siena_models[[x]][[i]]$theta),ncol=6)
    colnames(sens[[x]][[i]]) <- c('effect','theta','se','cut-off','model','unit')
    sens[[x]][[i]][,1] <- siena_models[[x]][[i]]$effects$effectName
    sens[[x]][[i]][,2] <- siena_models[[x]][[i]]$theta
    sens[[x]][[i]][,3] <- siena_models[[x]][[i]]$se
    sens[[x]][[i]][,4] <- ifelse(i %in% c(1,7,13),'10',
                                 ifelse(i %in% c(2,8,14),'15',
                                        ifelse(i %in% c(3,9,15),'20',
                                               ifelse(i %in% c(4,10,16),'25',
                                                      ifelse(i %in% c(5,11,17),'30','35')))))
    sens[[x]][[i]][,5] <- paste('Model',x,sep=' ')
    sens[[x]][[i]][,6] <- ifelse(i %in% 1:6,'Unit A',ifelse(i %in% 7:12,'Unit B','Unit C'))
    
  }
}
    
sens[[1]] <- do.call('rbind',sens[[1]])    
sens[[2]] <- do.call('rbind',sens[[2]])    
sens[[3]] <- do.call('rbind',sens[[3]])    
sens <- do.call('rbind',sens)
sens <- as.data.frame(sens)

# Only hypothesised effects
sens <- sens[sens$effect %in% c('gos_pos','gos_neg','gos_inco',
                                'gos_simp_pos','gos_simp_neg','gos_ampl_pos','gos_ampl_neg',
                                'int.  ptargets ego x gos_pos','int.  ntargets ego x gos_neg'),]
sens <- sens[-which(sens$model == 'Model 3' & sens$effect %in% c('gos_pos','gos_neg')),]

sens$theta <- as.numeric(sens$theta)
sens$se <- as.numeric(sens$se)
sens$se <- ifelse(sens$se > 2,NA,sens$se) # if very big SE, don't plot it
sens$effect <- factor(sens$effect,
                      levels=c('gos_pos','gos_neg','gos_inco','gos_simp_pos','gos_ampl_pos',
                               'gos_simp_neg','gos_ampl_neg',
                               'int.  ptargets ego x gos_pos','int.  ntargets ego x gos_neg'),
                      labels=c('Pos.','Neg.','Pos. & neg.',
                               'Pos. (one)','Pos. (several)',
                               'Neg. (one)','Neg. (several)',
                               'Pos. x pos. targets (ego)',
                               'Neg. x neg. targets (ego)'))

no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='Figure4.jpeg',width=13,height=7,units='in',res=1000)
ggplot(data=sens)+
  geom_errorbar(aes(x=effect,y=theta,ymin=theta-qnorm(.95)*se,ymax=theta+qnorm(.95)*se,colour=`cut-off`),
                position=position_dodge(width=.75),size=.75,alpha=.75)+
  geom_point(aes(x=effect,y=theta,colour=`cut-off`,shape=`cut-off`),
             size=2.5,position=position_dodge(width=.75))+
  scale_colour_manual(values=c('gray60','gray50','red','gray40','gray30','gray20'))+
  geom_hline(yintercept=0)+
  facet_wrap(model~unit,scales='free')+
  xlab('')+ylab('')+
  theme(axis.text.y=element_text(angle=50,hjust=1))+
  no.background
dev.off()

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