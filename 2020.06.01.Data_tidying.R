########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## Data tidying
## R script written by J. Luis Estevez (University of Groningen)
## Date: June 1st, 2019
########################################################################################################################

# R PACKAGES REQUIRED
library(readxl)
library(igraph)

########################################################################################################################

# DATA LOADING
covariates <- list()
covariates[['W1']] <- read_excel('covariates_w12.xlsx',sheet='W1',col_names=TRUE,na='NA')
covariates[['W2']] <- read_excel('covariates_w12.xlsx',sheet='W2',col_names=TRUE,na='NA')
covariates <- lapply(covariates,arrange,ID)

friends <- list()
friends[['AW1']] <- read_excel('friends_w123.xlsx',sheet='W1A',col_names=TRUE,na='NA')[,-1]
friends[['BW1']] <- read_excel('friends_w123.xlsx',sheet='W1B',col_names=TRUE,na='NA')[,-1]
friends[['CW1']] <- read_excel('friends_w123.xlsx',sheet='W1C',col_names=TRUE,na='NA')[,-1]
friends[['AW2']] <- read_excel('friends_w123.xlsx',sheet='W2A',col_names=TRUE,na='NA')[,-1]
friends[['BW2']] <- read_excel('friends_w123.xlsx',sheet='W2B',col_names=TRUE,na='NA')[,-1]
friends[['CW2']] <- read_excel('friends_w123.xlsx',sheet='W2C',col_names=TRUE,na='NA')[,-1]
friends[['AW3']] <- read_excel('friends_w123.xlsx',sheet='W3A',col_names=TRUE,na='NA')[,-1]
friends[['BW3']] <- read_excel('friends_w123.xlsx',sheet='W3B',col_names=TRUE,na='NA')[,-1]
friends[['CW3']] <- read_excel('friends_w123.xlsx',sheet='W3C',col_names=TRUE,na='NA')[,-1]

gossip <- list()
gossip[['A']] <- read_excel('gossip_w1.xlsx',sheet='A',col_names=TRUE,na='NA')
gossip[['B']] <- read_excel('gossip_w1.xlsx',sheet='B',col_names=TRUE,na='NA')
gossip[['C']] <- read_excel('gossip_w1.xlsx',sheet='C',col_names=TRUE,na='NA')

comm <- list()
comm[['A']] <- read_excel('comm_w1.xlsx',sheet='A',col_names=TRUE,na='NA')[,-1]
comm[['B']] <- read_excel('comm_w1.xlsx',sheet='B',col_names=TRUE,na='NA')[,-1]
comm[['C']] <- read_excel('comm_w1.xlsx',sheet='C',col_names=TRUE,na='NA')[,-1]

########################################################################################################################

# DATA TIDYING
# Some variables defined as factors
for(x in seq_along(covariates)){
  for(i in c('ID','dept','setting','team','gender','answer')){
    covariates[[x]][[i]] <- as.factor(covariates[[x]][[i]])
  }
}

# Relational data (friendship and communication) to matrices
for(i in seq_along(friends)){
  friends[[i]] <- as.matrix(friends[[i]])
  rownames(friends[[i]]) <- colnames(friends[[i]])
  diag(friends[[i]]) <- NA
}
for(i in seq_along(comm)){
  comm[[i]] <- as.matrix(comm[[i]])
  rownames(comm[[i]]) <- colnames(comm[[i]])
  diag(comm[[i]]) <- NA
}

# Removal of gossip exchanges missing the tone
gossip[['A']] <- gossip$A[!is.na(gossip$A$tone),] # 1 case in Unit A
gossip[['B']] <- gossip$B[!is.na(gossip$B$tone),] # 9 cases in Unit B
gossip[['C']] <- gossip$C[!is.na(gossip$C$tone),] # 1 case in Unit C

# Exclusion of board directors (Raad van Bestuur) from the sample
bd <- as.character(unique(covariates$W1[covariates$W1$dept=='Raad van Bestuur',]$ID))
'%!in%' <- function(x,y)!('%in%'(x,y))

# Exclusion of board directors in sociodemographic data
for(i in seq_along(covariates)){
  covariates[[i]] <- covariates[[i]][covariates[[i]]$ID %!in% bd,]
}
# Exclusion of board director in friendship networks
for(i in seq_along(friends)){
  if(bd[1] %in% rownames(friends[[i]]) | bd[2] %in% rownames(friends[[i]])){
    friends[[i]] <- friends[[i]][-which(rownames(friends[[i]]) %in% as.character(bd)),
                                 -which(colnames(friends[[i]]) %in% as.character(bd))]
  }
}
# Exclusion of board director in communications networks
for(i in seq_along(comm)){
  if(bd[1] %in% rownames(comm[[i]]) | bd[2] %in% rownames(comm[[i]])){
    comm[[i]] <- comm[[i]][-which(rownames(comm[[i]]) %in% as.character(bd)),
                           -which(colnames(comm[[i]]) %in% as.character(bd))]
  }
}

# Exclusion of triplets involving a board director in the gossip data
for(x in seq_along(gossip)){
  gossip[[x]] <- gossip[[x]][ifelse(gossip[[x]]$receiver %in% bd | 
                                      gossip[[x]]$sender %in% bd | 
                                      gossip[[x]]$target %in% bd,FALSE,TRUE),]
}

########################################################################################################################

# TRANSFORMATIONS

# Friendship: dichotomisation
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

friends2 <- lapply(friends,dichotomise,zero=c(1:3,98), # 98 (I don't know this person to 0)
                   one=c(4,5),na=c(9,97)) # 97 (I don't know) to missing

miss_noimp <- c(sum(is.na(friends2$AW2)),sum(is.na(friends2$BW2)),sum(is.na(friends2$CW2)))
ties_noimp <- c(sum(friends2$AW2,na.rm=TRUE),sum(friends2$BW2,na.rm=TRUE),sum(friends2$CW2,na.rm=TRUE))

# Imputation of friendship ties from wave 3 to wave 2
for(i in rownames(friends$AW2)){
  for(j in colnames(friends$AW2)){
    if(is.na(friends$AW2[i,j]) | friends$AW2[i,j] == 97){
      if(i %in% rownames(friends$AW3) & j %in% colnames(friends$AW3)){
        friends$AW2[i,j] <- friends$AW3[i,j]
      }
    }
  }
}
for(i in rownames(friends$BW2)){
  for(j in colnames(friends$BW2)){
    if(is.na(friends$BW2[i,j]) | friends$BW2[i,j] == 97){
      if(i %in% rownames(friends$BW3) & j %in% colnames(friends$BW3)){
        friends$BW2[i,j] <- friends$BW3[i,j]
      }
    }
  }
}
for(i in rownames(friends$CW2)){
  for(j in colnames(friends$CW2)){
    if(is.na(friends$CW2[i,j]) | friends$CW2[i,j] == 97){
      if(i %in% rownames(friends$CW3) & j %in% colnames(friends$CW3)){
        friends$CW2[i,j] <- friends$CW3[i,j]
      }
    }
  }
}

friends <- friends[1:6]
friends2 <- lapply(friends,dichotomise,zero=c(1:3,98),one=c(4,5),na=c(9,97))

# Cells imputed
miss_imp <- c(sum(is.na(friends2$AW2)),sum(is.na(friends2$BW2)),sum(is.na(friends2$CW2)))
miss_noimp - miss_imp
# Friendship ties imputed
ties_imp <- c(sum(friends2$AW2,na.rm=TRUE),sum(friends2$BW2,na.rm=TRUE),sum(friends2$CW2,na.rm=TRUE))
ties_imp - ties_noimp

# Multiverse approach (out-degree correction based on cut-off)
friends_00 <- lapply(friends,dichotomise,zero=c(1:4,98),one=5,na=c(9,97))
friends_05 <- friends2
friends_10 <- friends2
friends_15 <- friends2
friends_20 <- friends2
friends_25 <- friends2
friends_30 <- friends2
friends_35 <- friends2

for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 5
  friends_05[[x]][od,] <- friends_00[[x]][od,]
}
for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 10
  friends_10[[x]][od,] <- friends_00[[x]][od,]
}
for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 15
  friends_15[[x]][od,] <- friends_00[[x]][od,]
}
for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 20
  friends_20[[x]][od,] <- friends_00[[x]][od,]
}
for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 25
  friends_25[[x]][od,] <- friends_00[[x]][od,]
}
for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 30
  friends_30[[x]][od,] <- friends_00[[x]][od,]
}
for(x in seq_along(friends2)){
  od <- rowSums(friends2[[x]],na.rm=TRUE) > 35
  friends_35[[x]][od,] <- friends_00[[x]][od,]
}

friendsM <- list(friends_00,friends_05,friends_10,friends_15,friends_20,friends_25,friends_30,friends_35)
names(friendsM) <- c(0,5,10,15,20,25,30,35)
rm(friends_00);rm(friends_05);rm(friends_10);rm(friends_15)
rm(friends_20);rm(friends_25);rm(friends_30);rm(friends_35)

# Proportion of respondents whose ties are corrected
prop_corr <- matrix(NA,nrow=7,ncol=6)
rownames(prop_corr) <- c(5,10,15,20,25,30,35)
colnames(prop_corr) <- c('A t1','B t1','C t1','A t2','B t2','C t2')

for(x in rownames(prop_corr)){
  for(y in 1:ncol(prop_corr)){
    prop_corr[x,y] <- mean(rowSums(friends2[[y]],na.rm=TRUE) > as.numeric(x))
  }
}
write.table(prop_corr,'prop_corr.csv',sep=';')

# Communications matrices remain weighted
# Value 98 ('I don't know that person') -> 1 ('never interacted with j');
# Value 97 ('I don't know') -> NA
comm <- lapply(comm,dichotomise,zero=0,one=98,na=97)

# Addition of the structural missing
# All nodes per unit (no matter if they stayed, left, or enrolled between waves)
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

for(x in seq_along(friendsM)){
  friendsM[[x]][c('AW1','AW2')] <- lapply(friendsM[[x]][c('AW1','AW2')],enlarge,unitA,NA)
  friendsM[[x]][c('BW1','BW2')] <- lapply(friendsM[[x]][c('BW1','BW2')],enlarge,unitB,NA)
  friendsM[[x]][c('CW1','CW2')] <- lapply(friendsM[[x]][c('CW1','CW2')],enlarge,unitC,NA)
}

comm$A <- enlarge(comm$A,unitA,NA)
comm$B <- enlarge(comm$B,unitB,NA)
comm$C <- enlarge(comm$C,unitC,NA)

########################################################################################################################

# GOSSIP, DATA TRANSFORMATIONS

# Projections: Receiver-target gossip networks (positive and negative separately)
gossip$Ap <- as.matrix(get.adjacency(graph.data.frame(gossip$A[gossip$A$tone == '+',c('receiver','target')])))
gossip$An <- as.matrix(get.adjacency(graph.data.frame(gossip$A[gossip$A$tone %in% c('-','mix'),c('receiver','target')])))
gossip$Bp <- as.matrix(get.adjacency(graph.data.frame(gossip$B[gossip$B$tone == '+',c('receiver','target')])))
gossip$Bn <- as.matrix(get.adjacency(graph.data.frame(gossip$B[gossip$B$tone %in% c('-','mix'),c('receiver','target')])))
gossip$Cp <- as.matrix(get.adjacency(graph.data.frame(gossip$C[gossip$C$tone == '+',c('receiver','target')])))
gossip$Cn <- as.matrix(get.adjacency(graph.data.frame(gossip$C[gossip$C$tone %in% c('-','mix'),c('receiver','target')])))

# Nodes present in wave 1
unitA1 <- as.character(covariates$W1[covariates$W1$setting == 'Unit A',]$ID)
unitB1 <- as.character(covariates$W1[covariates$W1$setting == 'Unit B',]$ID)
unitC1 <- as.character(covariates$W1[covariates$W1$setting == 'Unit C',]$ID)

# Enlargement with zeroes for those present in wave 1
gossip$Ap <- enlarge(gossip$Ap,unitA1,fillwith=0)
gossip$An <- enlarge(gossip$An,unitA1,fillwith=0)
gossip$Bp <- enlarge(gossip$Bp,unitB1,fillwith=0)
gossip$Bn <- enlarge(gossip$Bn,unitB1,fillwith=0)
gossip$Cp <- enlarge(gossip$Cp,unitC1,fillwith=0)
gossip$Cn <- enlarge(gossip$Cn,unitC1,fillwith=0)

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
gossip$Bp <- gossip$Bp[unitB1c,]
gossip$Bn <- gossip$Bn[unitB1c,]
gossip$Cp <- gossip$Cp[unitC1c,]
gossip$Cn <- gossip$Cn[unitC1c,]

# Enlargement with NA 
gossip$Ap <- enlarge(gossip$Ap,unitA,NA)
gossip$An <- enlarge(gossip$An,unitA,NA)
gossip$Bp <- enlarge(gossip$Bp,unitB,NA)
gossip$Bn <- enlarge(gossip$Bn,unitB,NA)
gossip$Cp <- enlarge(gossip$Cp,unitC,NA)
gossip$Cn <- enlarge(gossip$Cn,unitC,NA)

# Diagonal to NA
for(i in 4:length(gossip)){
  diag(gossip[[i]]) <- NA
}

########################################################################################################################

rm(prop_corr);rm(bd);rm(i);rm(j);rm(miss_imp);rm(miss_noimp);rm(od);rm(ties_imp);rm(ties_noimp);rm(x);rm(y)

########################################################################################################################

# Save image
save.image('tidieddata.RData')
