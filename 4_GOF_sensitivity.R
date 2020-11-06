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
load('siena_results.RData')

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
                                'ptargets ego','ntargets ego',
                                'int.  ptargets ego x gos_pos','int.  ntargets ego x gos_neg'),]

# Removal of Model 1
sens <- sens[sens$model != "Model 1",]

# Distinction between with and without reinforcement in Model 2, and between main, prevalence, and interaction in Model 3
sens[sens$effect %in% c('gos_simp_pos','gos_simp_neg'),]$model <- "Model 2 (Without reinforcement)"
sens[sens$effect %in% c('gos_ampl_pos','gos_ampl_neg'),]$model <- "Model 2 (With reinforcement)"
sens[sens$effect %in% c('gos_pos','gos_neg'),]$model <- "Model 3 (Main effect)"
sens[sens$effect %in% c('ptargets ego','ntargets ego'),]$model <- "Model 3 (Prevalence effect)"
sens[sens$effect %in% c('int.  ptargets ego x gos_pos','int.  ntargets ego x gos_neg'),]$model <- "Model 3 (Interaction)"
sens$model <- factor(sens$model,
                     levels=c('Model 2 (Without reinforcement)','Model 2 (With reinforcement)',
                              'Model 3 (Main effect)','Model 3 (Prevalence effect)','Model 3 (Interaction)'))

sens$theta <- as.numeric(sens$theta)
sens$se <- as.numeric(sens$se)

# Visualisation
sens$effect <- factor(sens$effect,
                      levels=c('gos_pos','gos_neg','gos_simp_pos','gos_simp_neg','gos_ampl_pos','gos_ampl_neg',
                               'ptargets ego','ntargets ego',
                               'int.  ptargets ego x gos_pos','int.  ntargets ego x gos_neg'),
                      labels=rep(c('Positive','Negative'),5))

no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='Figure4.jpeg',width=13,height=7,units='in',res=1000)
ggplot(data=sens)+
  geom_hline(yintercept=0,alpha=.5)+
  geom_pointrange(aes(x=effect,y=theta,ymin=theta-qnorm(.975)*se,ymax=theta+qnorm(.975)*se,colour=`cut-off`),
                position=position_dodge(width=.75),size=.75)+
  scale_colour_manual(values=c('gray70','gray55','red','gray40','gray25','gray10'))+
  facet_wrap(unit~model,scales='free',nrow=3)+
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