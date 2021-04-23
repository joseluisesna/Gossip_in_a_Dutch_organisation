########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## Descriptive analysis (2)
## R script written by Jose Luis Estevez (Linkoping University)
## Date: October 28th, 2020
########################################################################################################################

# R PACKAGES REQUIRED
library(tidyverse);library(sna);library(igraph);library(plot3D)

########################################################################################################################

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

write.table(sum_w1,'sum_w1.csv',sep=';')
write.table(sum_w2,'sum_w2.csv',sep=';')

########################################################################################################################

# RESPONSE RATE

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

write.table(resp,'response_rate.csv',sep=';')

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

# DESCRIPTIVE ANALYSIS OF FRIENDSHIP (friends_20): CROSS-SECTIONAL

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

fri_desc$A1 <- describeNet(friendsM$`20`$AW1,unitA1)
fri_desc$A2 <- describeNet(friendsM$`20`$AW2,unitA2)
fri_desc$B1 <- describeNet(friendsM$`20`$BW1,unitB1)
fri_desc$B2 <- describeNet(friendsM$`20`$BW2,unitB2)
fri_desc$C1 <- describeNet(friendsM$`20`$CW1,unitC1)
fri_desc$C2 <- describeNet(friendsM$`20`$CW2,unitC2)

write.table(fri_desc,'fri_desc.csv',sep=';')

########################################################################################################################

# DESCRIPTIVE ANALYSIS OF FRIENDSHIP (friends_20): LONGITUDINAL

table(friendsM$`20`$AW1,friendsM$`20`$AW2,useNA='always')
table(friendsM$`20`$BW1,friendsM$`20`$BW2,useNA='always')
table(friendsM$`20`$CW1,friendsM$`20`$CW2,useNA='always')

# Jaccard index
Jaccard <- function(changetable) {
  return(changetable['1','1']/(changetable['0','1']+changetable['1','0']+changetable['1','1']))
}

Jaccard(table(friendsM$`20`$AW1,friendsM$`20`$AW2))
Jaccard(table(friendsM$`20`$BW1,friendsM$`20`$BW2))
Jaccard(table(friendsM$`20`$CW1,friendsM$`20`$CW2))

# Ties changed made per subject
A_change <- friendsM$`20`$AW1 + friendsM$`20`$AW2
A_change <- dichotomise(A_change,zero=c(0,2),one=1,na=NA)
A_change <- rowSums(A_change,na.rm=TRUE)

B_change <- friendsM$`20`$BW1 + friendsM$`20`$BW2
B_change <- dichotomise(B_change,zero=c(0,2),one=1,na=NA)
B_change <- rowSums(B_change,na.rm=TRUE)

C_change <- friendsM$`20`$CW1 + friendsM$`20`$CW2
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
jpeg(filename='Figure1.jpeg',width=9,height=3.5,units='in',res=1000)
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

# Projections
gossipS <- gossip[4:9] # Simple gossip
gossipC <- gossipS # Complex gossip
gossipI <- list() # Incongruent gossipa

gossipS <- lapply(gossipS,dichotomise,zero=0,one=1:10,na=NA)
gossipI[['A']] <- dichotomise(gossipS$Ap + gossipS$An,zero=0:1,one=2,na=NA)
gossipI[['B']] <- dichotomise(gossipS$Bp + gossipS$Bn,zero=0:1,one=2,na=NA)
gossipI[['C']] <- dichotomise(gossipS$Cp + gossipS$Cn,zero=0:1,one=2,na=NA)

gossipS$Ap <- gossipS$Ap - gossipI$A
gossipS$An <- gossipS$An - gossipI$A
gossipS$Bp <- gossipS$Bp - gossipI$B
gossipS$Bn <- gossipS$Bn - gossipI$B
gossipS$Cp <- gossipS$Cp - gossipI$C
gossipS$Cn <- gossipS$Cn - gossipI$C

gossipC <- lapply(gossipC,dichotomise,zero=0:1,one=2:10,na=NA)
gossipC$Ap <- gossipC$Ap - gossipI$A
gossipC$An <- gossipC$An - gossipI$A
gossipC$Bp <- gossipC$Bp - gossipI$B
gossipC$Bn <- gossipC$Bn - gossipI$B
gossipC$Cp <- gossipC$Cp - gossipI$C
gossipC$Cn <- gossipC$Cn - gossipI$C
gossipC <- lapply(gossipC,dichotomise,zero=c(-1,0),one=1,na=NA) # Careful with -1

# Summary table
gos_desc <- as.data.frame(matrix(NA,nrow=5,ncol=3,dimnames=list(c('pos','posR','neg','negR','both'),c('A','B','C'))))
gos_desc$A <- c(sum(gossipS$Ap,na.rm=TRUE),sum(gossipC$Ap,na.rm=TRUE),
                sum(gossipS$An,na.rm=TRUE),sum(gossipC$An,na.rm=TRUE),sum(gossipI$A,na.rm=TRUE))
gos_desc$B <- c(sum(gossipS$Bp,na.rm=TRUE),sum(gossipC$Bp,na.rm=TRUE),
                sum(gossipS$Bn,na.rm=TRUE),sum(gossipC$Bn,na.rm=TRUE),sum(gossipI$B,na.rm=TRUE))
gos_desc$C <- c(sum(gossipS$Cp,na.rm=TRUE),sum(gossipC$Cp,na.rm=TRUE),
                sum(gossipS$Cn,na.rm=TRUE),sum(gossipC$Cn,na.rm=TRUE),sum(gossipI$C,na.rm=TRUE))

write.table(gos_desc,'gos_desc.csv',sep=';')

# Extraction of gossip outdegrees (hearing about many targets equally)
gossip_deg <- lapply(gossip[4:9],dichotomise,zero=0,one=1:10,na=NA)
gossip_deg <- lapply(gossip_deg,sna::degree,cmode='outdegree')
unlist(lapply(gossip_deg,mean))
unlist(lapply(gossip_deg,range))

########################################################################################################################

# BIVARIATE ANALYSIS: CHANGES IN FRIENDSHIP BY (TYPE OF) GOSSIP HEARD
bivariate.desc <- function(friend1,friend2,spg,sng,cpg,cng,mix){
  no <- friend1
  pos <- friend1
  pos.re <- friend1
  neg <- friend1
  neg.re <- friend1
  both <- friend1
  
  # no gossip
  for(i in rownames(friend1)){
    for(j in colnames(friend1)){
      if((!(is.na(spg[i,j])) & spg[i,j]==1)|
         (!(is.na(sng[i,j])) & sng[i,j]==1)|
         (!(is.na(mix[i,j])) & mix[i,j]==1)){
        no[i,j] <- NA
      }
    }
  }
  # simple positive gossip
  for(i in rownames(friend1)){
    for(j in colnames(friend1)){
      if((!(is.na(spg[i,j])) & spg[i,j]==1)){
        pos[i,j] <- pos[i,j]
      }else{
        pos[i,j] <- NA
      }
    }
  }
  # amplified positive gossip
  for(i in rownames(friend1)){
    for(j in colnames(friend1)){
      if((!(is.na(cpg[i,j])) & cpg[i,j]==1)){
        pos.re[i,j] <- pos.re[i,j]
      }else{
        pos.re[i,j] <- NA
      }
    }
  }  
  # simple negative gossip
  for(i in rownames(friend1)){
    for(j in colnames(friend1)){
      if((!(is.na(sng[i,j])) & sng[i,j]==1)){
        neg[i,j] <- neg[i,j]
      }else{
        neg[i,j] <- NA
      }
    }
  } 
  # amplified negative gossip
  for(i in rownames(friend1)){
    for(j in colnames(friend1)){
      if((!(is.na(cng[i,j])) & cng[i,j]==1)){
        neg.re[i,j] <- neg.re[i,j]
      }else{
        neg.re[i,j] <- NA
      }
    }
  }
  # incongruent gossip
  for(i in rownames(friend1)){
    for(j in colnames(friend1)){
      if((!(is.na(mix[i,j])) & mix[i,j]==1)){
        both[i,j] <- both[i,j]
      }else{
        both[i,j] <- NA
      }
    }
  }
  
  change <- as.matrix(cbind(
    as.vector(table(pos,friend2,useNA=NULL)),
    as.vector(table(pos.re,friend2,useNA=NULL)),
    as.vector(table(neg,friend2,useNA=NULL)),
    as.vector(table(neg.re,friend2,useNA=NULL)),
    as.vector(table(both,friend2,useNA=NULL)),
    as.vector(table(no,friend2,useNA=NULL)),
    as.vector(table(friend1,friend2,useNA=NULL))))
  colnames(change) <- c('positive','pos.ampl','negative','neg.ampl','incongr','no','total')
  rownames(change) <- c('0->0','1->0','0->1','1->1')
  return(change[c(3,2,4,1),])
}

# Evolution of the network by type of gossip
(biv_1 <- bivariate.desc(friend1=friendsM$`20`$AW1,friend2=friendsM$`20`$AW2,
                         spg=gossipS$Ap,cpg=gossipC$Ap,sng=gossipS$An,cng=gossipC$An,mix=gossipI$A))
(biv_2 <- bivariate.desc(friend1=friendsM$`20`$BW1,friend2=friendsM$`20`$BW2,
                         spg=gossipS$Bp,cpg=gossipC$Bp,sng=gossipS$Bn,cng=gossipC$Bn,mix=gossipI$B))
(biv_3 <- bivariate.desc(friend1=friendsM$`20`$CW1,friend2=friendsM$`20`$CW2,
                         spg=gossipS$Cp,cpg=gossipC$Cp,sng=gossipS$Cn,cng=gossipC$Cn,mix=gossipI$C))

# Extraction of raw data (from bivariate analysis)
biv_1[,1] <- biv_1[,1] - biv_1[,2]
biv_1[,3] <- biv_1[,3] - biv_1[,4]
biv_2[,1] <- biv_2[,1] - biv_2[,2]
biv_2[,3] <- biv_2[,3] - biv_2[,4]
biv_3[,1] <- biv_3[,1] - biv_3[,2]
biv_3[,3] <- biv_3[,3] - biv_3[,4]

to_raw <- function(x){
  x <- cbind(Change=rep(rownames(x),x[,7]),
             Gossip=c(rep(colnames(x[,-7]),x[1,1:6]),rep(colnames(x[,-7]),x[2,1:6]),
                      rep(colnames(x[,-7]),x[3,1:6]),rep(colnames(x[,-7]),x[4,1:6])))
  return(as.data.frame(x))
}
biv <- list(biv_1,biv_2,biv_3)
biv <- lapply(biv,to_raw)

for(i in 1:length(biv)){
  if(i == 1){
    biv[[i]]$Unit <- 'Unit A'
  }else if(i == 2){
    biv[[i]]$Unit <- 'Unit B'
  }else{
    biv[[i]]$Unit <- 'Unit C'
  }
}

biv <- rbind(biv[[1]],biv[[2]],biv[[3]])

biv$Change <- factor(biv$Change,levels=c('0->1','1->0','1->1','0->0'),
                     labels=c('Created','Broken','Stable','Inexistent'))
biv$Gossip <- factor(biv$Gossip,levels=c('neg.ampl','negative','incongr','no','positive','pos.ampl'),
                     labels=c('negative (several senders)','negative (one sender)','positive and negative',
                              'no gossip','positive (one sender)','positive (several senders)'))

# Visualisation
no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

jpeg(filename='Figure2.jpeg',width=9,height=4,units='in',res=1000)
ggplot(data=biv)+
  geom_bar(aes(x=Change,fill=Gossip),position='fill',colour='black')+
  facet_wrap(~Unit)+
  scale_fill_manual(values = c('firebrick4','firebrick1','orange','gray98','chartreuse3','forestgreen'))+
  xlab('Friendships') + ylab('')+
  no.background
dev.off()

########################################################################################################################

# Save image
save.image('siena.RData')