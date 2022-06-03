########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## Data tidying (1)
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: May 29th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(igraph);library(ggplot2)

# DATA LOADING
rm(list=ls())
load('data.RData')

########################################################################################################################

# DATA TIDYING
# 1) ACTOR-LEVEL ATTRIBUTES
# Several variables are defined as factors
for(x in seq_along(covariates)){
  for(i in c('ID','dept','setting','team','gender','answer')){
    covariates[[x]][[i]] <- as.factor(covariates[[x]][[i]])
  }
}

# 2) NETWORK DATA
# Network data to a single object
ntw <- list(relation_quality=friends,communication=comm)
rm(friends);rm(comm)

# Turn into matrix objects
for(x in seq_along(ntw)){
  for(i in seq_along(ntw[[x]])){
    ntw[[x]][[i]] <- as.matrix(ntw[[x]][[i]])
    rownames(ntw[[x]][[i]]) <- colnames(ntw[[x]][[i]]) # rownames added
    diag(ntw[[x]][[i]]) <- NA # Diagonal sent to NA
  }
}

# 3) GOSSIP DATA
# Removal of gossip triplets with missing tone (1 case in Unit A, 9 cases in Unit B, and 1 case in Unit C)
for(x in seq_along(gossip)){
  gossip[[x]] <- gossip[[x]][!is.na(gossip[[x]]$tone),]
}

########################################################################################################################

# REMOVAL OF DATA CONCERNING BOARD OF DIRECTORS (NO USED FOR THIS STUDY)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function 'not in'
bd <- as.character(unique(covariates$W1[covariates$W1$dept=='Raad van Bestuur',]$ID)) # IDs of the 2 board directors

# Removal of board directors from actor-level data
for(i in seq_along(covariates)){
  covariates[[i]] <- covariates[[i]][covariates[[i]]$ID %!in% bd,]
}
# Removal of board directors from network data
for(x in seq_along(ntw)){
  for(i in seq_along(ntw[[x]])){
    ntw[[x]][[i]] <- ntw[[x]][[i]][rownames(ntw[[x]][[i]]) %!in% bd,colnames(ntw[[x]][[i]]) %!in% bd]
  }
}
# Removal of board directors from gossip data (if either the sender, receiver or target is a board director)
for(x in seq_along(gossip)){
  gossip[[x]] <- gossip[[x]][ifelse(gossip[[x]]$receiver %in% bd | gossip[[x]]$sender %in% bd | 
                                      gossip[[x]]$target %in% bd,FALSE,TRUE),]
}

########################################################################################################################

# DATA TRANSFORMATIONS
# Matrix dichotomisation (function)
dichotomise <- function(net,zero,one,na){
  for(i in rownames(net)){
    for(j in colnames(net)){
      if(!is.na(net[i,j]) & net[i,j] %in% zero){
        net[i,j] <- 0
      } else if(net[i,j] %in% one){
        net[i,j] <- 1
      } else if(net[i,j] %in% na)
        net[i,j] <- NA
    }
  }
  return(as.matrix(net))
}

# 1) FRIENDSHIP
ntw$friendship <- lapply(ntw$relation_quality,dichotomise,zero=c(1:3,98), 
                   one=c(4,5),na=c(9,97)) # 97 (I don't know) to missing

# Number of ties in time 1
for(i in 1:3){
  print(sum(ntw$friendship[[i]],na.rm=TRUE))
}
# Number of missing cells in time 1
for(i in 1:3){
  print(sum(is.na(ntw$friendship[[i]])) - nrow(ntw$friendship[[i]]))
}

# Number of ties in time 2
for(i in 4:6){
  print(sum(ntw$friendship[[i]],na.rm=TRUE))
}
# Number of missing cells in time 2
for(i in 4:6){
  print(sum(is.na(ntw$friendship[[i]])) - nrow(ntw$friendship[[i]]))
}

# DATA CORRECTION: Individuals with very large out-degree (>20), only 5 as friendships
# Wave 1
ntw$friendship$AW1[rowSums(ntw$friendship$AW1,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$AW1[rowSums(ntw$friendship$AW1,na.rm=TRUE) > 20,] == 5)
ntw$friendship$BW1[rowSums(ntw$friendship$BW1,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$BW1[rowSums(ntw$friendship$BW1,na.rm=TRUE) > 20,] == 5)
ntw$friendship$CW1[rowSums(ntw$friendship$CW1,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$CW1[rowSums(ntw$friendship$CW1,na.rm=TRUE) > 20,] == 5)
# Wave 2
ntw$friendship$AW2[rowSums(ntw$friendship$AW2,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$AW2[rowSums(ntw$friendship$AW2,na.rm=TRUE) > 20,] == 5)
ntw$friendship$BW2[rowSums(ntw$friendship$BW2,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$BW2[rowSums(ntw$friendship$BW2,na.rm=TRUE) > 20,] == 5)
ntw$friendship$CW2[rowSums(ntw$friendship$CW2,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$CW2[rowSums(ntw$friendship$CW2,na.rm=TRUE) > 20,] == 5)
# Wave 3
ntw$friendship$AW3[rowSums(ntw$friendship$AW3,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$AW3[rowSums(ntw$friendship$AW3,na.rm=TRUE) > 20,] == 5)
ntw$friendship$CW3[rowSums(ntw$friendship$CW3,na.rm=TRUE) > 20,] <- 1*(ntw$relation_quality$CW3[rowSums(ntw$friendship$CW3,na.rm=TRUE) > 20,] == 5)

# Number of ties in time 1
for(i in 1:3){
  print(sum(ntw$friendship[[i]],na.rm=TRUE))
}
# Number of missing cells in time 1
for(i in 1:3){
  print(sum(is.na(ntw$friendship[[i]])) - nrow(ntw$friendship[[i]]))
}

# Number of ties in time 2
for(i in 4:6){
  print(sum(ntw$friendship[[i]],na.rm=TRUE))
}
# Number of missing cells in time 2
for(i in 4:6){
  print(sum(is.na(ntw$friendship[[i]])) - nrow(ntw$friendship[[i]]))
}

# DATA IMPUTATION
# Imputation of ties in time 2
ntw$friendship_imp <- ntw$friendship[c('AW2','BW2','CW2')]

# If the dyad existed, and the tie (or absent thereof) was consistent in both times 1 and 3, we input this same value
for(i in rownames(ntw$friendship$AW2)){
  for(j in colnames(ntw$friendship$AW2)){
    if(is.na(ntw$friendship$AW2[i,j])){
      if(i %in% rownames(ntw$friendship$AW1) & j %in% colnames(ntw$friendship$AW1) & 
         i %in% rownames(ntw$friendship$AW3) & j %in% colnames(ntw$friendship$AW3)){
        if(!is.na(ntw$friendship$AW1[i,j]) & !is.na(ntw$friendship$AW3[i,j])){
          if(ntw$friendship$AW1[i,j] == 1 & ntw$friendship$AW3[i,j] == 1){
            ntw$friendship_imp$AW2[i,j] <- 1
          }else if(ntw$friendship$AW1[i,j] == 0 & ntw$friendship$AW3[i,j] == 0){
            ntw$friendship_imp$AW2[i,j] <- 0
          }
        }
      }
    }
  }
}
for(i in rownames(ntw$friendship$BW2)){
  for(j in colnames(ntw$friendship$BW2)){
    if(is.na(ntw$friendship$BW2[i,j])){
      if(i %in% rownames(ntw$friendship$BW1) & j %in% colnames(ntw$friendship$BW1) & 
         i %in% rownames(ntw$friendship$BW3) & j %in% colnames(ntw$friendship$BW3)){
        if(!is.na(ntw$friendship$BW1[i,j]) & !is.na(ntw$friendship$BW3[i,j])){
          if(ntw$friendship$BW1[i,j] == 1 & ntw$friendship$BW3[i,j] == 1){
            ntw$friendship_imp$BW2[i,j] <- 1
          }else if(ntw$friendship$BW1[i,j] == 0 & ntw$friendship$BW3[i,j] == 0){
            ntw$friendship_imp$BW2[i,j] <- 0
          }
        }
      }
    }
  }
}
for(i in rownames(ntw$friendship$CW2)){
  for(j in colnames(ntw$friendship$CW2)){
    if(is.na(ntw$friendship$CW2[i,j])){
      if(i %in% rownames(ntw$friendship$CW1) & j %in% colnames(ntw$friendship$CW1) & 
         i %in% rownames(ntw$friendship$CW3) & j %in% colnames(ntw$friendship$CW3)){
        if(!is.na(ntw$friendship$CW1[i,j]) & !is.na(ntw$friendship$CW3[i,j])){
          if(ntw$friendship$CW1[i,j] == 1 & ntw$friendship$CW3[i,j] == 1){
            ntw$friendship_imp$CW2[i,j] <- 1
          }else if(ntw$friendship$CW1[i,j] == 0 & ntw$friendship$CW3[i,j] == 0){
            ntw$friendship_imp$CW2[i,j] <- 0
          }
        }
      }
    }
  }
}

# Then, for dyads that did not exist in time 1, but did in time 3, we just imputted the value in time 3
for(i in rownames(ntw$friendship$AW2)){
  for(j in colnames(ntw$friendship$AW2)){
    if(is.na(ntw$friendship$AW2[i,j])){
      if((i %in% rownames(ntw$friendship$AW3) & j %in% colnames(ntw$friendship$AW3)) &
         (i %!in% rownames(ntw$friendship$AW1) | j %!in% colnames(ntw$friendship$AW1))){
        ntw$friendship_imp$AW2[i,j] <- ntw$friendship$AW3[i,j]
      }
    }
  }
}
for(i in rownames(ntw$friendship$BW2)){
  for(j in colnames(ntw$friendship$BW2)){
    if(is.na(ntw$friendship$BW2[i,j])){
      if((i %in% rownames(ntw$friendship$BW3) & j %in% colnames(ntw$friendship$BW3)) &
         (i %!in% rownames(ntw$friendship$BW1) | j %!in% colnames(ntw$friendship$BW1))){
        ntw$friendship_imp$BW2[i,j] <- ntw$friendship$BW3[i,j]
      }
    }
  }
}
for(i in rownames(ntw$friendship$CW2)){
  for(j in colnames(ntw$friendship$CW2)){
    if(is.na(ntw$friendship$CW2[i,j])){
      if((i %in% rownames(ntw$friendship$CW3) & j %in% colnames(ntw$friendship$CW3)) &
         (i %!in% rownames(ntw$friendship$CW1) | j %!in% colnames(ntw$friendship$CW1))){
        ntw$friendship_imp$CW2[i,j] <- ntw$friendship$CW3[i,j]
      }
    }
  }
}

# Number of ties in time 2 (after imputation)
for(i in 1:3){
  print(sum(ntw$friendship_imp[[i]],na.rm=TRUE))
}
# Number of missing cells in time 2 (after imputation)
for(i in 1:3){
  print(sum(is.na(ntw$friendship_imp[[i]])) - nrow(ntw$friendship_imp[[i]]))
}

# In unit A, ties grew from 190 to 222, and missing declined from 835 to 659 
# In Unit B, ties grew from 315 to 326, and missing declined from 1454 to 1208
# In Unit c, ties grew from 357 to 377, and missing declined from 596 to 516

# 2) COMMUNICATION FREQUENCY

# Since this variable is a control, it remains weighted. The only changes we introduced are these:
# Value 98 ('I don't know that person') -> 1 ('never interacted with j');
# Value 97 ('I don't know') -> NA
ntw$communication <- lapply(ntw$communication,dichotomise,zero=0,one=98,na=97)

########################################################################################################################

# MATRIX ENLARGEMENT (ADDITION OF STRUCTURAL MISSING)
# IDs of all the nodes per unit (no matter if they stayed, left, or enrolled between wave 1 and 2)
unitA <- sort(base::union(covariates$W1[covariates$W1$setting=='Unit A',]$ID,
                          covariates$W2[covariates$W2$setting=='Unit A',]$ID))
unitB <- sort(base::union(covariates$W1[covariates$W1$setting=='Unit B',]$ID,
                          covariates$W2[covariates$W2$setting=='Unit B',]$ID))
unitC <- sort(base::union(covariates$W1[covariates$W1$setting=='Unit C',]$ID,
                          covariates$W2[covariates$W2$setting=='Unit C',]$ID))

# Function to enlarge matrices
enlarge <- function(net,ids,fillwith=NA){
  x <- matrix(fillwith,nrow=length(ids),ncol=length(ids),dimnames=list(ids,ids))
  for(i in rownames(net)){
    for(j in colnames(net)){
      x[i,j] <- net[i,j]
    }
  }
  return(x)
}

# Enlargement of friendship networks
ntw$friendship[c('AW1','AW2')] <- lapply(ntw$friendship[c('AW1','AW2')],enlarge,unitA,NA)
ntw$friendship[c('BW1','BW2')] <- lapply(ntw$friendship[c('BW1','BW2')],enlarge,unitB,NA)
ntw$friendship[c('CW1','CW2')] <- lapply(ntw$friendship[c('CW1','CW2')],enlarge,unitC,NA)
ntw$friendship <- ntw$friendship[1:6]
# Enlargement of friendship networks (with imputed data)
ntw$friendship_imp['AW2'] <- lapply(ntw$friendship_imp['AW2'],enlarge,unitA,NA)
ntw$friendship_imp['BW2'] <- lapply(ntw$friendship_imp['BW2'],enlarge,unitB,NA)
ntw$friendship_imp['CW2'] <- lapply(ntw$friendship_imp['CW2'],enlarge,unitC,NA)
# Enlargement of communication frquency networks
ntw$communication['A'] <- lapply(ntw$communication['A'],enlarge,unitA,NA)
ntw$communication['B'] <- lapply(ntw$communication['B'],enlarge,unitB,NA)
ntw$communication['C'] <- lapply(ntw$communication['C'],enlarge,unitC,NA)

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
no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

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

# GOSSIP, DATA TRANSFORMATIONS

# If only want gossip from friends (it is a condition that the gossip receiver considers the sender a friend)
#gossip$A <- gossip$A[!is.na(gossip$A$friendshipRS) & gossip$A$friendshipRS == 1,]
#gossip$B <- gossip$B[!is.na(gossip$B$friendshipRS) & gossip$B$friendshipRS == 1,]
#gossip$C <- gossip$C[!is.na(gossip$C$friendshipRS) & gossip$C$friendshipRS == 1,]

# Projections: Receiver-target gossip networks (positive, negative and mixed gossip separately)
gossip$Ap <- as.matrix(get.adjacency(graph.data.frame(gossip$A[gossip$A$tone == '+',c('receiver','target')])))
gossip$An <- as.matrix(get.adjacency(graph.data.frame(gossip$A[gossip$A$tone == '-',c('receiver','target')])))
gossip$Am <- as.matrix(get.adjacency(graph.data.frame(gossip$A[gossip$A$tone == 'mix',c('receiver','target')])))
gossip$Bp <- as.matrix(get.adjacency(graph.data.frame(gossip$B[gossip$B$tone == '+',c('receiver','target')])))
gossip$Bn <- as.matrix(get.adjacency(graph.data.frame(gossip$B[gossip$B$tone == '-',c('receiver','target')])))
gossip$Bm <- as.matrix(get.adjacency(graph.data.frame(gossip$B[gossip$B$tone == 'mix',c('receiver','target')])))
gossip$Cp <- as.matrix(get.adjacency(graph.data.frame(gossip$C[gossip$C$tone == '+',c('receiver','target')])))
gossip$Cn <- as.matrix(get.adjacency(graph.data.frame(gossip$C[gossip$C$tone == '-',c('receiver','target')])))
gossip$Cm <- as.matrix(get.adjacency(graph.data.frame(gossip$C[gossip$C$tone == 'mix',c('receiver','target')])))

# Nodes present in wave 1
unitA1 <- as.character(covariates$W1[covariates$W1$setting == 'Unit A',]$ID)
unitB1 <- as.character(covariates$W1[covariates$W1$setting == 'Unit B',]$ID)
unitC1 <- as.character(covariates$W1[covariates$W1$setting == 'Unit C',]$ID)

# Enlargement with zeroes for those present in wave 1
gossip$Ap <- enlarge(gossip$Ap,unitA1,fillwith=0)
gossip$An <- enlarge(gossip$An,unitA1,fillwith=0)
gossip$Am <- enlarge(gossip$Am,unitA1,fillwith=0)
gossip$Bp <- enlarge(gossip$Bp,unitB1,fillwith=0)
gossip$Bn <- enlarge(gossip$Bn,unitB1,fillwith=0)
gossip$Bm <- enlarge(gossip$Bm,unitB1,fillwith=0)
gossip$Cp <- enlarge(gossip$Cp,unitC1,fillwith=0)
gossip$Cn <- enlarge(gossip$Cn,unitC1,fillwith=0)
gossip$Cm <- enlarge(gossip$Cm,unitC1,fillwith=0)

# If respondent was not present at wave 1, their outdegree is missing
# Structural missing to NA too
unitA1c <- as.character(na.omit(covariates$W1[covariates$W1$setting == 'Unit A' & 
                                                covariates$W1$answer == 'completed',]$ID))
unitB1c <- as.character(na.omit(covariates$W1[covariates$W1$setting == 'Unit B' & 
                                                covariates$W1$answer == 'completed',]$ID))
unitC1c <- as.character(na.omit(covariates$W1[covariates$W1$setting == 'Unit C' & 
                                                covariates$W1$answer == 'completed',]$ID))

gossip$Ap <- gossip$Ap[unitA1c,]
gossip$An <- gossip$An[unitA1c,]
gossip$Am <- gossip$Am[unitA1c,]
gossip$Bp <- gossip$Bp[unitB1c,]
gossip$Bn <- gossip$Bn[unitB1c,]
gossip$Bm <- gossip$Bm[unitB1c,]
gossip$Cp <- gossip$Cp[unitC1c,]
gossip$Cn <- gossip$Cn[unitC1c,]
gossip$Cm <- gossip$Cm[unitC1c,]

# Enlargement with NA 
gossip$Ap <- enlarge(gossip$Ap,unitA,NA)
gossip$An <- enlarge(gossip$An,unitA,NA)
gossip$Am <- enlarge(gossip$Am,unitA,NA)
gossip$Bp <- enlarge(gossip$Bp,unitB,NA)
gossip$Bn <- enlarge(gossip$Bn,unitB,NA)
gossip$Bm <- enlarge(gossip$Bm,unitB,NA)
gossip$Cp <- enlarge(gossip$Cp,unitC,NA)
gossip$Cn <- enlarge(gossip$Cn,unitC,NA)
gossip$Cm <- enlarge(gossip$Cm,unitC,NA)

# Diagonal to NA
for(i in 5:length(gossip)){
  diag(gossip[[i]]) <- NA
}

########################################################################################################################

rm(i);rm(j);rm(x);rm(bd)

########################################################################################################################

# Save image
save.image('tidieddata.RData')