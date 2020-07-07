########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## SAOM analysis
## R script written by J. Luis Estevez (University of Groningen)
## Date: June 4th, 2019
########################################################################################################################

# R PACKAGES REQUIRED
library(tidyverse)
library(RSiena)

########################################################################################################################

# DATA LOADING
load('siena.RData')

########################################################################################################################

# PREMILINARY CHECKS (NUMBER OF FRIENDSHIPS CHANGED BETWEEN WAVE 1 AND WAVE 2)

covA <- covariates2[covariates2$setting=='Unit A',]
covB <- covariates2[covariates2$setting=='Unit B',]
covC <- covariates2[covariates2$setting=='Unit C',]

# Ties changed between wave 1 and wave 2
covA$changes <- A_change
covB$changes <- B_change
covC$changes <- C_change

# Number of positive and negative targets 
covA$ptarg <- gossip_deg$Ap
covA$ntarg <- gossip_deg$An
covB$ptarg <- gossip_deg$Bp
covB$ntarg <- gossip_deg$Bn
covC$ptarg <- gossip_deg$Cp
covC$ntarg <- gossip_deg$Cn

# Out and in- friendship degree in wave 1
covA$outdegree <- rowSums(friendsM$`20`$AW1,na.rm=TRUE)
covA$indegree <- colSums(friendsM$`20`$AW1,na.rm=TRUE)
covB$outdegree <- rowSums(friendsM$`20`$BW1,na.rm=TRUE)
covB$indegree <- colSums(friendsM$`20`$BW1,na.rm=TRUE)
covC$outdegree <- rowSums(friendsM$`20`$CW1,na.rm=TRUE)
covC$indegree <- colSums(friendsM$`20`$CW1,na.rm=TRUE)

# One single dataset for all thre units
covariates2 <- rbind(covA,covB,covC)
covariates2$gender <- as.numeric(covariates2$gender=='female')

# Dataset for plotting purposes
plotdata <- gather(covariates2[,c('gender','age','tenure','workhours','outdegree','indegree','ptarg','ntarg')],
                   key=variable,value=answer)
plotdata$variable <- factor(plotdata$variable,
                            levels=c('gender','age','tenure','workhours','outdegree','indegree','ptarg','ntarg'),
                            labels=c('Gender (male/female)','Age','Tenure','Working hours per week',
                                     'Friendships (out)','Friendships (in)','Positive targets','Negative targets'))
plotdata$changes <- rep(covariates2$changes,8)
plotdata$Setting <- rep(covariates2$setting,8)

set.seed(123)
plotdata[plotdata$variable == 'Gender (male/female)',]$answer <- jitter(plotdata[plotdata$variable == 'Gender (male/female)',
                                                                                    ]$answer,amount=.05)
plotdata[plotdata$variable %in% c('Friendships (out)','Friendships (in)','Positive targets','Negative targets'),
         ]$answer <- jitter(plotdata[plotdata$variable %in% c('Friendships (out)','Friendships (in)',
                                                              'Positive targets','Negative targets'),]$answer,amount=.25)

# Visualisation
no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='Figure2.jpeg',width=9,height=7,units='in',res=1000)
ggplot(data=plotdata,aes(x=answer,y=changes,colour=Setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  facet_wrap(~variable,scales='free_x',nrow=2) +
  geom_smooth(method='loess',se=FALSE) +
  xlab('') + ylab('Friendships changed from wave 1 to wave 2') +
  ylim(c(0,max(plotdata$changes))) +
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  no.background
dev.off()

# OLS regression
regA <- glm(changes~gender+age+tenure+workhours+outdegree+indegree+ptarg+ntarg+team,
            data=covariates2[covariates2$setting == 'Unit A',],family=gaussian(link='identity'))
regB <- glm(changes~gender+age+tenure+workhours+outdegree+indegree+ptarg+ntarg+team,
            data=covariates2[covariates2$setting == 'Unit B',],family=gaussian(link='identity'))
regC <- glm(changes~gender+age+tenure+workhours+outdegree+indegree+ptarg+ntarg+team,
            data=covariates2[covariates2$setting == 'Unit C',],family=gaussian(link='identity'))
summary(regA);summary(regB);summary(regC)
# Residuals
par(mfrow=c(2,2))
plot(regA);plot(regB);plot(regC)
par(mfrow=c(1,1))

########################################################################################################################

# SAOM - OBJECT CREATION

# RESPONSE VARIABLE (FRIENDSHIP)
friends_W1 <- list(friendsM$`10`$AW1,friendsM$`15`$AW1,friendsM$`20`$AW1,friendsM$`25`$AW1,friendsM$`30`$AW1,friendsM$`35`$AW1,
                   friendsM$`10`$BW1,friendsM$`15`$BW1,friendsM$`20`$BW1,friendsM$`25`$BW1,friendsM$`30`$BW1,friendsM$`35`$BW1,
                   friendsM$`10`$CW1,friendsM$`15`$CW1,friendsM$`20`$CW1,friendsM$`25`$CW1,friendsM$`30`$CW1,friendsM$`35`$CW1)
friends_W2 <- list(friendsM$`10`$AW2,friendsM$`15`$AW2,friendsM$`20`$AW2,friendsM$`25`$AW2,friendsM$`30`$AW2,friendsM$`35`$AW2,
                   friendsM$`10`$BW2,friendsM$`15`$BW2,friendsM$`20`$BW2,friendsM$`25`$BW2,friendsM$`30`$BW2,friendsM$`35`$BW2,
                   friendsM$`10`$CW2,friendsM$`15`$CW2,friendsM$`20`$CW2,friendsM$`25`$CW2,friendsM$`30`$CW2,friendsM$`35`$CW2)
                
sienaDep <- vector('list',length(friends_W1))
for(i in 1:length(friends_W1)){
  sienaDep[[i]] <- sienaDependent(array(c(friends_W1[[i]],friends_W2[[i]]),dim=c(nrow(friends_W1[[i]]),ncol(friends_W2[[i]]),2)))
}

# TYPES OF GOSSIP
gos_pos <- list(coDyadCovar(gossipS$Ap,centered=TRUE),coDyadCovar(gossipS$Bp,centered=TRUE),coDyadCovar(gossipS$Cp,centered=TRUE))
gos_neg <- list(coDyadCovar(gossipS$An,centered=TRUE),coDyadCovar(gossipS$Bn,centered=TRUE),coDyadCovar(gossipS$Cn,centered=TRUE))
gos_simp_pos <- list(coDyadCovar(gossipS$Ap-gossipC$Ap,centered=TRUE),
                     coDyadCovar(gossipS$Bp-gossipC$Bp,centered=TRUE),
                     coDyadCovar(gossipS$Cp-gossipC$Cp,centered=TRUE))
gos_simp_neg <- list(coDyadCovar(gossipS$An-gossipC$An,centered=TRUE),
                     coDyadCovar(gossipS$Bn-gossipC$Bn,centered=TRUE),
                     coDyadCovar(gossipS$Cn-gossipC$Cn,centered=TRUE))
gos_ampl_pos <- list(coDyadCovar(gossipC$Ap,centered=TRUE),coDyadCovar(gossipC$Bp,centered=TRUE),coDyadCovar(gossipC$Cp,centered=TRUE))
gos_ampl_neg <- list(coDyadCovar(gossipC$An,centered=TRUE),coDyadCovar(gossipC$Bn,centered=TRUE),coDyadCovar(gossipC$Cn,centered=TRUE))
gos_inco <- list(coDyadCovar(gossipI$A,centered=TRUE),coDyadCovar(gossipI$B,centered=TRUE),coDyadCovar(gossipI$C,centered=TRUE))

# COVARIATES
gender <- list(coCovar(1*(covA$gender=='female'),centered=TRUE),
               coCovar(1*(covB$gender=='female'),centered=TRUE),
               coCovar(1*(covC$gender=='female'),centered=TRUE))
age <- list(coCovar(covA$age,centered=TRUE),coCovar(covB$age,centered=TRUE),coCovar(covC$age,centered=TRUE))
tenure <- list(coCovar(covA$tenure,centered=TRUE),coCovar(covB$tenure,centered=TRUE),coCovar(covC$tenure,centered=TRUE))
workhours <- list(coCovar(covA$workhours,centered=TRUE),coCovar(covB$workhours,centered=TRUE),coCovar(covC$workhours,centered=TRUE))
team <- list(coCovar(as.numeric(covA$team),centered=TRUE),
             coCovar(as.numeric(covB$team),centered=TRUE),
             coCovar(as.numeric(covC$team),centered=TRUE))
ptargets <- list(coCovar(covA$ptarg,centered=TRUE),coCovar(covB$ptarg,centered=TRUE),coCovar(covC$ptarg,centered=TRUE))
ntargets <- list(coCovar(covA$ntarg,centered=TRUE),coCovar(covB$ntarg,centered=TRUE),coCovar(covC$ntarg,centered=TRUE))
comm <- list(coDyadCovar(comm$A,centered=TRUE),coDyadCovar(comm$B,centered=TRUE),coDyadCovar(comm$C,centered=TRUE))

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

comp.file <- list(sienaCompositionChange(comp.change$A),sienaCompositionChange(comp.change$B),sienaCompositionChange(comp.change$C))

# RSIENA OBJECTS

sienaObj <- vector('list',length(friends_W1))
for(i in 1:6){
  sienaObj[[i]] <- sienaDataCreate(friendship=sienaDep[[i]],gos_pos=gos_pos[[1]],gos_neg=gos_neg[[1]],gos_simp_pos=gos_simp_pos[[1]],
                                   gos_simp_neg=gos_simp_neg[[1]],gos_ampl_pos=gos_ampl_pos[[1]],gos_ampl_neg=gos_ampl_neg[[1]],
                                   gos_inco=gos_inco[[1]],gender=gender[[1]],age=age[[1]],tenure=tenure[[1]],team=team[[1]],
                                   workhours=workhours[[1]],ptargets=ptargets[[1]],ntargets=ntargets[[1]],comm=comm[[1]],comp.file[[1]])
}
for(i in 7:12){
  sienaObj[[i]] <- sienaDataCreate(friendship=sienaDep[[i]],gos_pos=gos_pos[[2]],gos_neg=gos_neg[[2]],gos_simp_pos=gos_simp_pos[[2]],
                                   gos_simp_neg=gos_simp_neg[[2]],gos_ampl_pos=gos_ampl_pos[[2]],gos_ampl_neg=gos_ampl_neg[[2]],
                                   gos_inco=gos_inco[[2]],gender=gender[[2]],age=age[[2]],tenure=tenure[[2]],team=team[[2]],
                                   workhours=workhours[[2]],ptargets=ptargets[[2]],ntargets=ntargets[[2]],comm=comm[[2]],comp.file[[2]])
}
for(i in 13:18){
  sienaObj[[i]] <- sienaDataCreate(friendship=sienaDep[[i]],gos_pos=gos_pos[[3]],gos_neg=gos_neg[[3]],gos_simp_pos=gos_simp_pos[[3]],
                                   gos_simp_neg=gos_simp_neg[[3]],gos_ampl_pos=gos_ampl_pos[[3]],gos_ampl_neg=gos_ampl_neg[[3]],
                                   gos_inco=gos_inco[[3]],gender=gender[[3]],age=age[[3]],tenure=tenure[[3]],team=team[[3]],
                                   workhours=workhours[[3]],ptargets=ptargets[[3]],ntargets=ntargets[[3]],comm=comm[[3]],comp.file[[3]])
}

# Print report for every model
for(i in 1:length(sienaObj)){
  print01Report(sienaObj[[i]],modelname=paste('model',i))
}

########################################################################################################################

# SAOM - MODEL SPECIFICATION
siena_effects <- vector('list',length(sienaObj))
for(i in 1:length(sienaObj)){
  siena_effects[[i]] <- getEffects(sienaObj[[i]])
}

# Common effects to all models: structural and homophily effects 
for(i in 1:length(siena_effects)){
  siena_effects[[i]] <- includeEffects(siena_effects[[i]],gwespFF,inPopSqrt,outActSqrt,type='eval') 
  siena_effects[[i]] <- includeInteraction(siena_effects[[i]],recip,gwespFF,parameter=69,type='eval')
  siena_effects[[i]] <- includeEffects(siena_effects[[i]],altX,egoX,simX,interaction1='age',type='eval') 
  siena_effects[[i]] <- includeEffects(siena_effects[[i]],altX,egoX,simX,interaction1='tenure',type='eval') 
  siena_effects[[i]] <- includeEffects(siena_effects[[i]],altX,egoX,simX,interaction1='workhours',type='eval') 
  siena_effects[[i]] <- includeEffects(siena_effects[[i]],sameX,interaction1='team',type='eval') 
  siena_effects[[i]] <- includeEffects(siena_effects[[i]],X,interaction1='comm',type='eval') 
}

siena_effects1 <- siena_effects
siena_effects2 <- siena_effects
siena_effects3 <- siena_effects

# Model 1: positive, negative and incongruent gossip
for(i in 1:length(siena_effects1)){
  siena_effects1[[i]] <- includeEffects(siena_effects1[[i]],X,interaction1='gos_pos',type='eval')
  siena_effects1[[i]] <- includeEffects(siena_effects1[[i]],X,interaction1='gos_neg',type='eval')
  siena_effects1[[i]] <- includeEffects(siena_effects1[[i]],X,interaction1='gos_inco',type='eval')
}

# Model 2: simple vs. reinforced gossip
for(i in 1:length(siena_effects2)){
  siena_effects2[[i]] <- includeEffects(siena_effects2[[i]],X,interaction1='gos_simp_pos',type='eval')
  siena_effects2[[i]] <- includeEffects(siena_effects2[[i]],X,interaction1='gos_simp_neg',type='eval')
  siena_effects2[[i]] <- includeEffects(siena_effects2[[i]],X,interaction1='gos_ampl_pos',type='eval')
  siena_effects2[[i]] <- includeEffects(siena_effects2[[i]],X,interaction1='gos_ampl_neg',type='eval')
}

# Model 3: Interaction with number of targets 
for(i in 1:length(siena_effects3)){
  siena_effects3[[i]] <- includeEffects(siena_effects3[[i]],X,interaction1='gos_pos',type='eval')
  siena_effects3[[i]] <- includeEffects(siena_effects3[[i]],X,interaction1='gos_neg',type='eval')
  siena_effects3[[i]] <- includeEffects(siena_effects3[[i]],egoX,interaction1='ptargets',type='eval') 
  siena_effects3[[i]] <- includeEffects(siena_effects3[[i]],egoX,interaction1='ntargets',type='eval') 
  siena_effects3[[i]] <- includeInteraction(siena_effects3[[i]],egoX,X,interaction1=c('ptargets','gos_pos'),type='eval') # pos. targets x all pos. gossip 
  siena_effects3[[i]] <- includeInteraction(siena_effects3[[i]],egoX,X,interaction1=c('ntargets','gos_neg'),type='eval')
}

# ALGORITHM
algorithm1 <- sienaAlgorithmCreate(projname='results1',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm2 <- sienaAlgorithmCreate(projname='results2',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm3 <- sienaAlgorithmCreate(projname='results3',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)

siena_models1 <- list()
siena_models2 <- list()
siena_models3 <- list()

# SAOM - RESULTS
for(i in 1:length(sienaObj)){
  siena_models1[[i]] <- siena07(algorithm1,data=sienaObj[[i]],effects=siena_effects1[[i]],
                                batch=FALSE,verbose=FALSE,useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
}
for(i in 1:length(sienaObj)){
  siena_models2[[i]] <- siena07(algorithm2,data=sienaObj[[i]],effects=siena_effects2[[i]],
                                batch=FALSE,verbose=FALSE,useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
}
for(i in 1:length(sienaObj)){
  siena_models3[[i]] <- siena07(algorithm3,data=sienaObj[[i]],effects=siena_effects3[[i]],
                                batch=FALSE,verbose=FALSE,useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
}

save.image('siena_results.RData')

########################################################################################################################

# PRESENTATION OF RESULTS

# Results as table
sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*',
                       ifelse(x < .1,'.',''))))
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

# Results for cut-off 20
write.table(outcome(siena_models1[c(3,9,15)]),'siena_models1_20.csv',sep=';')
write.table(outcome(siena_models2[c(3,9,15)]),'siena_models2_20.csv',sep=';')
write.table(outcome(siena_models3[c(3,9,15)]),'siena_models3_20.csv',sep=';')
