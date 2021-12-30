########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## Descriptive analysis (2)
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: December 28th, 2021
########################################################################################################################

# R PACKAGES REQUIRED
library(tidyverse);library(tidyr);library(sna);library(igraph);library(plot3D);library(ggpubr)

# DATA LOADING
rm(list=ls())
load('tidieddata.RData')

########################################################################################################################

# UNIVARIATE DESCRIPTIVE STATISTICS
# Summary of covariates (wave 1)
sum_w1 <- covariates$W1 %>%
  group_by(setting) %>%
  summarise(gender = sum(gender == 'female')/length(gender),
            age_avg = mean(age,na.rm=TRUE),
            age_m = min(age,na.rm=TRUE),
            age_M = max(age,na.rm=TRUE),
            tenure_avg = mean(tenure,na.rm=TRUE),
            tenure_m = min(tenure,na.rm=TRUE),
            tenure_M = max(tenure,na.rm=TRUE),
            wh_avg = mean(workhours,na.rm=TRUE),
            wh_m = min(workhours,na.rm=TRUE),
            wh_M = max(workhours,na.rm=TRUE),
            n_team = length(unique(team)))

# Summary of covariates (wave 2)
sum_w2 <- covariates$W2 %>%
  group_by(setting) %>%
  summarise(gender = sum(gender == 'female')/length(gender),
            age_avg = mean(age,na.rm=TRUE),
            age_m = min(age,na.rm=TRUE),
            age_M = max(age,na.rm=TRUE),
            tenure_avg = mean(tenure,na.rm=TRUE),
            tenure_m = min(tenure,na.rm=TRUE),
            tenure_M = max(tenure,na.rm=TRUE),
            wh_avg = mean(workhours,na.rm=TRUE),
            wh_m = min(workhours,na.rm=TRUE),
            wh_M = max(workhours,na.rm=TRUE),
            n_team = length(unique(team)))

write.table(sum_w1,'sum_w1.csv',sep=',',row.names=FALSE)
write.table(sum_w2,'sum_w2.csv',sep=',',row.names=FALSE)

########################################################################################################################

# CREATION OF A DATASET WITH COVARIATES, REGARDLESS OF IN WHICH WAVE THEY WERE COLLECTED
covariates2 <- merge(covariates$W1[,c('ID','setting','team','gender','age','tenure','workhours','startyear')],
                     covariates$W2[,c('ID','setting','team','gender','age','tenure','workhours','startyear',
                                      'startmonth','startday')],
                     by=c('ID','setting'),all=TRUE)

# Gender, age, and startyear stay the same. Just collapsed them in a single variable
for(i in seq_along(covariates2$ID)){
  if(is.na(covariates2$gender.x[i])){
    covariates2$gender.x[i] <- covariates2$gender.y[i]
  }
  if(is.na(covariates2$age.x[i])){
    covariates2$age.x[i] <- covariates2$age.y[i]-.5 # half a year less
  }
  if(is.na(covariates2$startyear.x[i])){
    covariates2$startyear.x[i] <- covariates2$startyear.y[i]
  }
}

# tenure was recalculated using startyear, startmonth and startday (if missing, tenure)
covariates2$tenure <- round(2008+9/12 - (covariates2$startyear.x+covariates2$startmonth/12+covariates2$startday/365),2)
for(i in 1:nrow(covariates2)){
  if(is.na(covariates2$tenure[i])){
    covariates2$tenure[i] <- covariates2$tenure.x[i]
  }
}
for(i in 1:nrow(covariates2)){
  if(is.na(covariates2$tenure[i])){
    covariates2$tenure[i] <- covariates2$tenure.y[i]
  }
}

# The number of working hours changed for some individuals from wave 1 to wave 2 
which(covariates2$workhours.x!=covariates2$workhours.y)
# Thus I used the mean when two different working hours where reported
# If the respondent only filled once the questionnaire, I used that number
for(i in seq_along(covariates2$ID)){
  if(is.na(covariates2$workhours.x[i])){
    covariates2$workhours.x[i] <- covariates2$workhours.y[i]
  }
  if (i %in% which(!(covariates2$workhours.x == covariates2$workhours.y))){
    covariates2$workhours.x[i] <- mean(c(covariates2$workhours.x[i],covariates2$workhours.y[i]),na.rm=TRUE)
  }
}

# As for teams, I use as a reference the team reported in wave 1. 
# Only those who did not report team in wave 1, or joined between waves, were assigned the team reported in wave 2
for(i in seq_along(covariates2$ID)){
  if(is.na(covariates2$team.x[i])){
    covariates2$team.x[i] <- covariates2$team.y[i]
  }
}

# this is only for plotting reasons (later on)
for(i in seq_along(covariates2$ID)){
  if(is.na(covariates2$team.y[i])){
    covariates2$team.y[i] <- covariates2$team.x[i]
  }
}

covariates2 <- covariates2[,c('ID','setting','gender.x','age.x','tenure','workhours.x','team.x','team.y')]
names(covariates2) <- c('ID','setting','gender','age','tenure','workhours','team','team2')

########################################################################################################################

# MISSING DATA: RESPONSE RATES
resp <- as.data.frame(matrix(NA,nrow=3,ncol=7))
rownames(resp) <- c('wave 1','wave 2','wave 3')
colnames(resp) <- c('N','n_1','resp_1','n_2','resp_2','n_both','resp_both')

resp$N <- c(length(unitA),length(unitB),length(unitC)) # total sample 
resp$n_1 <- c(length(unitA1),length(unitB1),length(unitC1)) # sample in wave 1
resp$resp_1 <- c(length(unitA1c),length(unitB1c),length(unitC1c)) # respondents in wave 1

unitA2 <- as.character(covariates$W2[covariates$W2$setting == 'Unit A',]$ID)
unitB2 <- as.character(covariates$W2[covariates$W2$setting == 'Unit B',]$ID)
unitC2 <- as.character(covariates$W2[covariates$W2$setting == 'Unit C',]$ID)

unitA2c <- as.character(na.omit(covariates$W2[covariates$W2$setting == 'Unit A' & covariates$W2$answer == 'completed',]$ID))
unitB2c <- as.character(na.omit(covariates$W2[covariates$W2$setting == 'Unit B' & covariates$W2$answer == 'completed',]$ID))
unitC2c <- as.character(na.omit(covariates$W2[covariates$W2$setting == 'Unit C' & covariates$W2$answer == 'completed',]$ID))

resp$n_2 <- c(length(unitA2),length(unitB2),length(unitC2)) # sample in wave 1
resp$resp_2 <- c(length(unitA2c),length(unitB2c),length(unitC2c)) # respondents in wave 2

# sample of those present at  both waves
resp$n_both <- c(length(intersect(unitA1,unitA2)),
                 length(intersect(unitB1,unitB2)),
                 length(intersect(unitC1,unitC2)))
resp$resp_both <- c(length(intersect(unitA1c,unitA2c)),
                    length(intersect(unitB1c,unitB2c)),
                    length(intersect(unitC1c,unitC2c)))

write.table(resp,'response_rate.csv',sep=',',row.names=FALSE)

# RELATIONSHIP BETWEEN MISSINGNESS (LEAVING THE ORGANISATION), AND FRIENDSHIP AND GOSSIP OUT-DEGREE 
# How many people befriend me or receive pos/neg/mixed gossip about me, compared to other colleagues
dgrs <- rbind(
data.frame(ID=unitA1,
           friendship=colSums(ntw$friendship$AW1,na.rm = TRUE)[unitA1],
           pos_gossip=colSums(1*(gossip$Ap != 0),na.rm=TRUE)[unitA1],
           neg_gossip=colSums(1*(gossip$An != 0),na.rm=TRUE)[unitA1],
           mix_gossip=colSums(1*(gossip$Am != 0),na.rm=TRUE)[unitA1],
           unit='A'),
data.frame(ID=unitB1,
           friendship=colSums(ntw$friendship$BW1,na.rm = TRUE)[unitB1],
           pos_gossip=colSums(1*(gossip$Bp != 0),na.rm=TRUE)[unitB1],
           neg_gossip=colSums(1*(gossip$Bn != 0),na.rm=TRUE)[unitB1],
           mix_gossip=colSums(1*(gossip$Bm != 0),na.rm=TRUE)[unitB1],
           unit='B'),
data.frame(ID=unitC1,
           friendship=colSums(ntw$friendship$CW1,na.rm = TRUE)[unitC1],
           pos_gossip=colSums(1*(gossip$Cp != 0),na.rm=TRUE)[unitC1],
           neg_gossip=colSums(1*(gossip$Cn != 0),na.rm=TRUE)[unitC1],
           mix_gossip=colSums(1*(gossip$Cm != 0),na.rm=TRUE)[unitC1],
           unit='C'))

# Individuals who left between t1 and t2
exiting <- c(unitA1[unitA1 %!in% unitA2],unitB1[unitB1 %!in% unitB2],unitC1[unitC1 %!in% unitC2])
dgrs$exiting <- as.factor(ifelse(dgrs$ID %in% exiting,1,0))

t.test(dgrs[dgrs$exiting == 0,]$friendship,
       dgrs[dgrs$exiting == 1,]$friendship)
t.test(dgrs[dgrs$exiting == 0,]$pos_gossip,
       dgrs[dgrs$exiting == 1,]$pos_gossip)
t.test(dgrs[dgrs$exiting == 0,]$neg_gossip,
       dgrs[dgrs$exiting == 1,]$neg_gossip)
t.test(dgrs[dgrs$exiting == 0,]$mix_gossip,
       dgrs[dgrs$exiting == 1,]$mix_gossip)

# Visualisation
dgrs <- gather(dgrs,key='type',value='Outdegree',-c('ID','unit','exiting'))

dgrs$unit <- factor(dgrs$unit,levels=c('A','B','C'),labels=c('Unit A','Unit B','Unit C'))
dgrs$type <- factor(dgrs$type,levels=c('friendship','pos_gossip','neg_gossip','mix_gossip'),
       labels=c('Friendship','Positive gossip','Negative gossip','Mixed gossip'))
dgrs$Employee <- factor(dgrs$exiting,levels=c(0,1),labels=c('Stayed','Left'))

source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")
p2data <- "https://raw.githubusercontent.com/datavizpyr/data/master/palmer_penguin_species.tsv"
penguins_df <- read_tsv(p2data)

no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='exiting.jpeg',width=7,height=7,units='in',res=500)
ggplot(data=dgrs,aes(x=1,y=Outdegree,fill=Employee))+ 
  geom_point(aes(color=Employee),position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75))+
  geom_flat_violin(position = position_nudge(x=.25,y=0),adjust=1,trim=FALSE,alpha=.35)+
  geom_boxplot(width=.4,alpha=.5)+
  facet_wrap(unit~type)+
  xlab('')+labs(fill='',colour='')+
  scale_colour_manual(values = c('dodgerblue','firebrick2'))+
  scale_fill_manual(values = c('steelblue','tomato'))+
  no.background +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(legend.position = "bottom", legend.justification = "center")+
  ylim(c(-5,20))
dev.off()

########################################################################################################################

# DESCRIPTIVE ANALYSIS OF FRIENDSHIP: CROSS-SECTIONAL
describeNet <- function(mtx,nodes){
  x <- array()
  mtx <- mtx[nodes,nodes]
  x[1] <- nrow(mtx) # nodes
  x[2] <- 1 - sum(!is.na(mtx)) / (nrow(mtx)*(nrow(mtx)-1)) # missing-tie
  x[3] <- sna::gden(mtx) # density
  x[4] <- sna::grecip(mtx,measure='edgewise') # reciprocity
  x[5] <- igraph::transitivity(graph_from_adjacency_matrix(mtx)) # transitiviy
  x[6] <- sum(sna::degree(mtx,cmode='freeman') == 0) # isolates 
  x[7] <- mean(sna::degree(mtx,cmode='outdegree')) # outdegree
  x[8] <- min(sna::degree(mtx,cmode='outdegree')) 
  x[9] <- max(sna::degree(mtx,cmode='outdegree')) 
  x[10] <- mean(sna::degree(mtx,cmode='indegree')) # indegree 
  x[11] <- min(sna::degree(mtx,cmode='indegree'))  
  x[12] <- max(sna::degree(mtx,cmode='indegree'))  
  return(x)
}

fri_desc <- as.data.frame(matrix(NA,nrow=12,ncol=6,
                                 dimnames=list(c('n','missing','density','recip','trans','isolates',
                                                 'outdegree_m','outdegree_min','outdegree_max',
                                                 'indegree_m','indegree_min','indegree_max'),
                                               c('A1','A2','B1','B2','C1','C2'))))

fri_desc$A1 <- describeNet(ntw$friendship$AW1,unitA1)
fri_desc$A2 <- describeNet(ntw$friendship_imp$AW2,unitA2)
fri_desc$B1 <- describeNet(ntw$friendship$BW1,unitB1)
fri_desc$B2 <- describeNet(ntw$friendship_imp$BW2,unitB2)
fri_desc$C1 <- describeNet(ntw$friendship$CW1,unitC1)
fri_desc$C2 <- describeNet(ntw$friendship_imp$CW2,unitC2)

write.table(fri_desc,'friendship_desc.csv',sep=',',row.names=TRUE)

########################################################################################################################

# DESCRIPTIVE ANALYSIS OF FRIENDSHIP: LONGITUDINAL
table(ntw$friendship$AW1,ntw$friendship_imp$AW2,useNA='always')
table(ntw$friendship$BW1,ntw$friendship_imp$BW2,useNA='always')
table(ntw$friendship$CW1,ntw$friendship_imp$CW2,useNA='always')

# Jaccard index
Jaccard <- function(changetable) {
  return(changetable['1','1']/(changetable['0','1']+changetable['1','0']+changetable['1','1']))
}

Jaccard(table(ntw$friendship$AW1,ntw$friendship_imp$AW2))
Jaccard(table(ntw$friendship$BW1,ntw$friendship_imp$BW2))
Jaccard(table(ntw$friendship$CW1,ntw$friendship_imp$CW2))

# Ties changed made per subject
A_change <- ntw$friendship$AW1 + ntw$friendship_imp$AW2
A_change <- dichotomise(A_change,zero=c(0,2),one=1,na=NA)
A_change <- rowSums(A_change,na.rm=TRUE)

B_change <- ntw$friendship$BW1 + ntw$friendship_imp$BW2
B_change <- dichotomise(B_change,zero=c(0,2),one=1,na=NA)
B_change <- rowSums(B_change,na.rm=TRUE)

C_change <- ntw$friendship$CW1 + ntw$friendship_imp$CW2
C_change <- dichotomise(C_change,zero=c(0,2),one=1,na=NA)
C_change <- rowSums(C_change,na.rm=TRUE)

# Average number of ties changed
mean(A_change);sd(A_change);min(A_change);max(A_change)
mean(B_change);sd(B_change);min(B_change);max(B_change)
mean(C_change);sd(C_change);min(C_change);max(C_change)
# At least one change?
sum(A_change > 0);sum(B_change > 0);sum(C_change > 0)

########################################################################################################################

# DESCRIPTIVE ANALYSIS OF GOSSIP
# Number of (positive/negative/mixed) gossip triads per unit
nrow(gossip$A[gossip$A$tone == '+',])
nrow(gossip$A[gossip$A$tone == '-',])
nrow(gossip$A[gossip$A$tone == 'mix',])

nrow(gossip$B[gossip$B$tone == '+',])
nrow(gossip$B[gossip$B$tone == '-',])
nrow(gossip$B[gossip$B$tone == 'mix',])

nrow(gossip$C[gossip$C$tone == '+',])
nrow(gossip$C[gossip$C$tone == '-',])
nrow(gossip$C[gossip$C$tone == 'mix',])

# Visualisation of the gossip triads
jpeg(filename='gossip cubes.jpeg',width=9,height=3.5,units='in',res=1000)
par(mfrow=c(1,3))
scatter3D(x=gossip$A$receiver,y=gossip$A$sender,z=gossip$A$target,
          col=ifelse(gossip$A$tone=='+','forestgreen',ifelse(gossip$A$tone=='-','red','darkorange2')),alpha=2/3,
          main='Unit A',xlab='Receiver',ylab='Sender',zlab='Target',
          bty='g',pch=16,cex=1,colkey=FALSE,theta=45,phi=30)
scatter3D(x=gossip$B$receiver,y=gossip$B$sender,z=gossip$B$target,
          col=ifelse(gossip$B$tone=='+','forestgreen',ifelse(gossip$B$tone=='-','red','darkorange2')),alpha=2/3,
          main='Unit B',xlab='Receiver',ylab='Sender',zlab='Target',
          bty='g',pch=16,cex=1,colkey=FALSE,theta=45,phi=30)
scatter3D(x=gossip$C$receiver,y=gossip$C$sender,z=gossip$C$target,
          col=ifelse(gossip$C$tone=='+','forestgreen',ifelse(gossip$C$tone=='-','red','darkorange2')),alpha=2/3,
          main='Unit C',xlab='Receiver',ylab='Sender',zlab='Target',
          bty='g',pch=16,cex=1,colkey=FALSE,theta=45,phi=30)
dev.off()

# Negative and mixed gossip are merged into a single category
gossip$Anm <- gossip$An + gossip$Am
gossip$Bnm <- gossip$Bn + gossip$Bm
gossip$Cnm <- gossip$Cn + gossip$Cm

# Projections (all, simple, and complex gossip)
gossipA <- gossipS <- gossipC <- gossip[c(4:5,7:8,10:11,13:15)] # for pos., neg., and the mixture of neg. and mixed gossip)
gossipI <- list() # Incongruent gossip (when heard both positve and neg. or mixed)

gossipA <- lapply(gossipA,dichotomise,zero=0,one=1:30,na=NA)
gossipS <- lapply(gossipS,dichotomise,zero=c(0,2:30),one=1,na=NA)
gossipC <- lapply(gossipC,dichotomise,zero=0:1,one=2:30,na=NA)

gossipI[['A']] <- dichotomise(gossipA$Ap + gossipA$Anm,zero=0:1,one=2,na=NA)
gossipI[['B']] <- dichotomise(gossipA$Bp + gossipA$Bnm,zero=0:1,one=2,na=NA)
gossipI[['C']] <- dichotomise(gossipA$Cp + gossipA$Cnm,zero=0:1,one=2,na=NA)

# Summary table
gos_desc <- as.data.frame(matrix(NA,nrow=5,ncol=3,dimnames=list(c('pos','posR','neg','negR','both'),c('A','B','C'))))
gos_desc$A <- c(sum(gossipA$Ap,na.rm=TRUE),sum(gossipC$Ap,na.rm=TRUE),
                sum(gossipA$Anm,na.rm=TRUE),sum(gossipC$Anm,na.rm=TRUE),sum(gossipI$A,na.rm=TRUE))
gos_desc$B <- c(sum(gossipA$Bp,na.rm=TRUE),sum(gossipC$Bp,na.rm=TRUE),
                sum(gossipA$Bnm,na.rm=TRUE),sum(gossipC$Bnm,na.rm=TRUE),sum(gossipI$B,na.rm=TRUE))
gos_desc$C <- c(sum(gossipA$Cp,na.rm=TRUE),sum(gossipC$Cp,na.rm=TRUE),
                sum(gossipA$Cnm,na.rm=TRUE),sum(gossipC$Cnm,na.rm=TRUE),sum(gossipI$C,na.rm=TRUE))

write.table(gos_desc,'gossip_desc.csv',sep=',')

# Extraction of gossip outdegrees (hearing about many targets equally)
gossip_deg <- lapply(gossip[c(4,7,10,13:15)],dichotomise,zero=0,one=1:30,na=NA)
gossip_deg <- lapply(gossip_deg,sna::degree,cmode='outdegree')
unlist(lapply(gossip_deg,mean))
unlist(lapply(gossip_deg,range))

########################################################################################################################

# DOES GOSSIP COME FROM FRIENDS (Sender)? IS POSITIVE GOSSIP ABOUT FRIENDS, AND NEG. GOSSIP ABOUT NON-FRIENDS (Target)?
gossip$A$friendshipRS <- gossip$A$friendshipRT <- NA
gossip$B$friendshipRS <- gossip$B$friendshipRT <- NA
gossip$C$friendshipRS <- gossip$C$friendshipRT <- NA

# Assign every gossip triplet whether receiver and sender are friends or not, and whether receiver and target are friends or not 
for(i in seq_along(gossip$A$friendshipRS)){
  gossip$A$friendshipRS[i] <- ntw$friendship$AW1[paste(gossip$A$receiver[i]),paste(gossip$A$sender[i])]
  gossip$A$friendshipRT[i] <- ntw$friendship$AW1[paste(gossip$A$receiver[i]),paste(gossip$A$target[i])]
}
for(i in seq_along(gossip$B$friendshipRS)){
  gossip$B$friendshipRS[i] <- ntw$friendship$BW1[paste(gossip$B$receiver[i]),paste(gossip$B$sender[i])]
  gossip$B$friendshipRT[i] <- ntw$friendship$BW1[paste(gossip$B$receiver[i]),paste(gossip$B$target[i])]
}
for(i in seq_along(gossip$C$friendshipRS)){
  gossip$C$friendshipRS[i] <- ntw$friendship$CW1[paste(gossip$C$receiver[i]),paste(gossip$C$sender[i])]
  gossip$C$friendshipRT[i] <- ntw$friendship$CW1[paste(gossip$C$receiver[i]),paste(gossip$C$target[i])]
}

# Merge data from all three units
gossip$A$unit <- 'Unit A'
gossip$B$unit <- 'Unit B'
gossip$C$unit <- 'Unit C'
gossip$all <- rbind(gossip$A,gossip$B,gossip$C)

# Create a variable telling the type of relationships between receiver, sender, and target
gossip$all$relations <- ifelse(gossip$all$friendshipRS == 1 & gossip$all$friendshipRT == 1, 'Friendship with both',
                               ifelse(gossip$all$friendshipRS == 1 & gossip$all$friendshipRT == 0, 'Friendship with sender',
                                      ifelse(gossip$all$friendshipRS == 0 & gossip$all$friendshipRT == 1, 'Friendship with target',
                                             ifelse(gossip$all$friendshipRS == 0 & gossip$all$friendshipRT == 0, 'Friendship with neither',NA))))
gossip$all$relations[is.na(gossip$all$relations)] <- 'Tie missing'
gossip$all$relations <- factor(gossip$all$relations,
                               levels=c('Friendship with sender','Friendship with target','Friendship with both',
                                        'Friendship with neither','Tie missing'))

gossip$all$Tone <- factor(gossip$all$tone,levels=c('+','-','mix'),labels=c('Positive','Negative','Mixed'))

# Visualisation
jpeg(filename='type of gossip.jpeg',width=8,height=7,units='in',res=500)
ggplot(data=gossip$all)+
  geom_bar(aes(x=Tone,fill=relations),colour='black',position='stack',alpha=.6)+
  facet_wrap(~unit)+
  scale_fill_manual(values = c('dodgerblue','orange','chartreuse3','firebrick2','grey'))+
  xlab('Type of the gossip')+ylab('Count')+labs(fill='')+
  no.background+
  theme(legend.position="top", legend.justification="center")
dev.off()

########################################################################################################################

# BIVARIATE ANALYSIS: CHANGES IN FRIENDSHIP BY (TYPE OF) GOSSIP HEARD
# Create a matrix telling whether each friendship tie was created, broken, maintained, or never existed
change_fr  <- list(friendship1 = ntw$friendship[1:3],friendship2 = ntw$friendship_imp)

for(x in seq_along(change_fr$friendship1)){
  for(i in 1:nrow(change_fr$friendship1[[x]])){
    for(j in 1:ncol(change_fr$friendship1[[x]])){
      if(is.na(change_fr$friendship1[[x]][i,j]) | is.na(change_fr$friendship2[[x]][i,j])){
        change_fr$friendship1[[x]][i,j] <- NA
      }else if(change_fr$friendship1[[x]][i,j] == 0 & change_fr$friendship2[[x]][i,j] == 1){
        change_fr$friendship1[[x]][i,j] <- 'Created'
      }else if(change_fr$friendship1[[x]][i,j] == 0 & change_fr$friendship2[[x]][i,j] == 0){
        change_fr$friendship1[[x]][i,j] <- 'Inexistent'
      }else if(change_fr$friendship1[[x]][i,j] == 1 & change_fr$friendship2[[x]][i,j] == 1){
        change_fr$friendship1[[x]][i,j] <- 'Stable'
      }else if(change_fr$friendship1[[x]][i,j] == 1 & change_fr$friendship2[[x]][i,j] == 0){
        change_fr$friendship1[[x]][i,j] <- 'Broken'
      }
    }
  }
  change_fr$friendship1[[x]] <- factor(change_fr$friendship1[[x]],levels=c('Created','Broken','Stable','Inexistent'))
}
change_fr <- change_fr$friendship1

# Create a matrix telling whether for each dyad, one received positive, negative, or both types of gossip (plus reinforcing)
gossiptype <- list(positive = gossip[c('Ap','Bp','Cp')],negaitve = gossip[c('Anm','Bnm','Cnm')])

for(x in seq_along(gossiptype$positive)){
  for(i in 1:nrow(gossiptype$positive[[x]])){
    for(j in 1:ncol(gossiptype$negaitve[[x]])){
      if(!is.na(gossiptype$positive[[x]][i,j])){
        if(gossiptype$positive[[x]][i,j] > 0 & gossiptype$negaitve[[x]][i,j] > 0){
          gossiptype$positive[[x]][i,j] <- 'Both'
        }else if(gossiptype$positive[[x]][i,j] > 1 & gossiptype$negaitve[[x]][i,j] == 0){
          gossiptype$positive[[x]][i,j] <- 'Pos (several)'
        }else if(gossiptype$positive[[x]][i,j] == 1 & gossiptype$negaitve[[x]][i,j] == 0){
          gossiptype$positive[[x]][i,j] <- 'Pos'
        }else if(gossiptype$positive[[x]][i,j] == 0 & gossiptype$negaitve[[x]][i,j] > 1){
          gossiptype$positive[[x]][i,j] <- 'Neg (several)'
        }else if(gossiptype$positive[[x]][i,j] == 0 & gossiptype$negaitve[[x]][i,j] == 1){
          gossiptype$positive[[x]][i,j] <- 'Neg'
        }else{
          gossiptype$positive[[x]][i,j] <- 'Neither'
        }
      }
    }
  }
  gossiptype$positive[[x]] <- factor(gossiptype$positive[[x]],
                                     levels=c('Pos','Pos (several)','Neg','Neg (several)','Both','Neither'))
}
gossiptype <- gossiptype$positive

# Creation of tables containing results
desc_biv <- list()
desc_biv$A <- as.data.frame.matrix(table(change_fr$AW1,gossiptype$Ap))
desc_biv$B <- as.data.frame.matrix(table(change_fr$BW1,gossiptype$Bp))
desc_biv$C <- as.data.frame.matrix(table(change_fr$CW1,gossiptype$Cp))

for(i in seq_along(desc_biv)){
  desc_biv[[i]]$Total <- rowSums(desc_biv[[i]]) # Addition of total per row
  desc_biv[[i]]$Pos <- desc_biv[[i]]$Pos + desc_biv[[i]]$`Pos (several)` # Pos is pos + pos from several
  desc_biv[[i]]$Neg <- desc_biv[[i]]$Neg + desc_biv[[i]]$`Neg (several)` # Neg is neg + neg from several
}

write.table(rbind(desc_biv$A,desc_biv$B,desc_biv$C),'bivariate.csv',sep=',',row.names=TRUE)

########################################################################################################################

# CHECK WHETHER CERTAIN INDIVIDUALS WERE MORE OR LESS LIKELY TO CHANGE FRIENDSHIPS (CREATE OR DELETE)
# Create covariates: number of friendships change, friendship outdegree (t1), friendship indegree (t1)
covariates2$fr_change <- NA
covariates2$fr_out <- NA
covariates2$fr_in <- NA
# Create covariates: number of positive gossip targets, number of negative gossip targets
covariates2$pos_targets <- NA
covariates2$neg_targets <- NA

# Organise the covariates by ID
covariates2$ID <- as.numeric(as.character(covariates2$ID))
covariates2 <- covariates2[order(covariates2$ID),]

# Number of friendships changed
covariates2[covariates2$setting == 'Unit A',]$fr_change <- rowSums(abs(ntw$friendship_imp$AW2 - ntw$friendship$AW1),na.rm=TRUE)
covariates2[covariates2$setting == 'Unit B',]$fr_change <- rowSums(abs(ntw$friendship_imp$BW2 - ntw$friendship$BW1),na.rm=TRUE)
covariates2[covariates2$setting == 'Unit C',]$fr_change <- rowSums(abs(ntw$friendship_imp$CW2 - ntw$friendship$CW1),na.rm=TRUE)
# Friendship outdegree in t1
covariates2[covariates2$setting == 'Unit A',]$fr_out <- rowSums(ntw$friendship$AW1,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit B',]$fr_out <- rowSums(ntw$friendship$BW1,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit C',]$fr_out <- rowSums(ntw$friendship$CW1,na.rm=TRUE)
# Friendship indegree in t1
covariates2[covariates2$setting == 'Unit A',]$fr_in <- colSums(ntw$friendship$AW1,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit B',]$fr_in <- colSums(ntw$friendship$BW1,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit C',]$fr_in <- colSums(ntw$friendship$CW1,na.rm=TRUE)
# Number of positive gossip targets
covariates2[covariates2$setting == 'Unit A',]$pos_targets <- rowSums(gossipA$Ap,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit B',]$pos_targets <- rowSums(gossipA$Bp,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit C',]$pos_targets <- rowSums(gossipA$Cp,na.rm=TRUE)
# Number of negative gossip targets
covariates2[covariates2$setting == 'Unit A',]$neg_targets <- rowSums(gossipA$Anm,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit B',]$neg_targets <- rowSums(gossipA$Bnm,na.rm=TRUE)
covariates2[covariates2$setting == 'Unit C',]$neg_targets <- rowSums(gossipA$Cnm,na.rm=TRUE)

# Visualisation
p1 <- ggplot(data=covariates2,aes(x=gender,y=fr_change,colour=setting,fill=setting)) +
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75))+
  geom_boxplot(colour='black',width=.4,alpha=.75)+
  geom_smooth(method='loess',se=FALSE) +
  xlab('Gender') + ylab('Friendships changed between waves 1 and 2')+labs(colour='',fill='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  scale_fill_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p2 <- ggplot(data=covariates2,aes(x=age,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Age') + ylab('')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p3 <- ggplot(data=covariates2,aes(x=tenure,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Tenure') + ylab('')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p4 <- ggplot(data=covariates2,aes(x=workhours,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Working hours') + ylab('')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p5 <- ggplot(data=covariates2,aes(x=fr_out,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Friendship outdegree') + ylab('Friendships changed between waves 1 and 2')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p6 <- ggplot(data=covariates2,aes(x=fr_in,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Friendship indegree') + ylab('')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p7 <- ggplot(data=covariates2,aes(x=pos_targets,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Positive targets') + ylab('')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

p8 <- ggplot(data=covariates2,aes(x=neg_targets,y=fr_change,colour=setting)) +
  geom_point(size=3,colour='black',alpha=.7) + 
  geom_point(size=1.8,alpha=.9) + 
  geom_smooth(method='loess',se=FALSE) +
  xlab('Negative targets') + ylab('')+labs(colour='')+
  scale_colour_manual(values = c('chartreuse3','firebrick2','dodgerblue'))+
  ylim(c(0,18))+
  no.background

jpeg(filename='friendships changed.jpeg',width=9,height=7,units='in',res=500)
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,
          common.legend = TRUE,
          nrow=2,ncol=4)
dev.off()

# REGRESSION TEST
# Make clear that teams in Unit A are not same teams in Unit B or C
covariates2$team <- as.character(covariates2$team)
unique(covariates2[covariates2$setting == 'Unit A',]$team) # no team number 7
covariates2[covariates2$setting == 'Unit A' & !is.na(covariates2$team) & covariates2$team == '8',]$team <- '7' # 8 as 7
covariates2[covariates2$setting == 'Unit B',]$team <- as.character(as.numeric(covariates2[covariates2$setting == 'Unit B',]$team) + 7)
covariates2[covariates2$setting == 'Unit C',]$team <- as.character(as.numeric(covariates2[covariates2$setting == 'Unit C',]$team) + 16)

regA <- glm(data=covariates2[covariates2$setting == 'Unit A',],
            fr_change~gender+age+tenure+workhours+fr_out+fr_in+pos_targets+neg_targets+team,
            family=gaussian(link='identity'))
regB <- glm(data=covariates2[covariates2$setting == 'Unit B',],
            fr_change~gender+age+tenure+workhours+fr_out+fr_in+pos_targets+neg_targets+team,
            family=gaussian(link='identity'))
regC <- glm(data=covariates2[covariates2$setting == 'Unit C',],
            fr_change~gender+age+tenure+workhours+fr_out+fr_in+pos_targets+neg_targets+team,
            family=gaussian(link='identity'))
summary(regA);summary(regB);summary(regC)

# Visualisation of residuals
jpeg(filename='reg Unit A.jpeg',width=7,height=7,units='in',res=500)
par(mfrow=c(2,2))
plot(regA)
dev.off()

jpeg(filename='reg Unit B.jpeg',width=7,height=7,units='in',res=500)
par(mfrow=c(2,2))
plot(regB)
dev.off()

jpeg(filename='reg Unit C.jpeg',width=7,height=7,units='in',res=500)
par(mfrow=c(2,2))
plot(regC)
dev.off()

par(mfrow=c(1,1))

########################################################################################################################

rm(change_fr);rm(desc_biv);rm(dgrs);rm(fri_desc);rm(GeomFlatViolin);rm(gos_desc);rm(gossip_deg);rm(gossiptype)
rm(penguins_df);rm(resp);rm(sum_w1);rm(sum_w2);rm(A_change);rm(B_change);rm(C_change);rm(exiting);rm(i);rm(j);rm(p2data);
rm(unitA1c);rm(unitB1c);rm(unitC1c);rm(unitA2c);rm(unitB2c);rm(unitC2c);rm(x);rm(describeNet);rm(dichotomise);rm(enlarge);
rm(geom_flat_violin);rm(Jaccard);rm(p1);rm(p2);rm(p3);rm(p4);rm(p5);rm(p6);rm(p7);rm(p8);rm(regA);rm(regB);rm(regC)

########################################################################################################################

# Save image
save.image('sienadata.RData')