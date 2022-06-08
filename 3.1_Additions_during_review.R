########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## Additions based on reviewers' comments (3.1)
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: June 8th, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(igraph);library(statnet)

# DATA LOADING
rm(list=ls())
load('sienadata.RData')

########################################################################################################################

# QAP REGRESSION (CHANGES IN FRIENDSHIP VS. COMMUNICATION FREQUENCY)

# (1) Ties created
A_tiescreated <- ntw$friendship_imp$AW2 - ntw$friendship$AW1 # substract existing ties from W2
A_tiescreated[!is.na(A_tiescreated) & A_tiescreated == -1] <- 0 # send minus ones to zeroes

B_tiescreated <- ntw$friendship_imp$BW2 - ntw$friendship$BW1 
B_tiescreated[!is.na(B_tiescreated) & B_tiescreated == -1] <- 0 

C_tiescreated <- ntw$friendship_imp$CW2 - ntw$friendship$CW1 
C_tiescreated[!is.na(C_tiescreated) & C_tiescreated == -1] <- 0 

# (2) Ties broken 
A_tiesbroken <- ntw$friendship$AW1 - ntw$friendship_imp$AW2 # substract ties that remains from W1
A_tiesbroken[!is.na(A_tiesbroken) & A_tiesbroken == -1] <- 0 # send minus ones to zeroes

B_tiesbroken <- ntw$friendship$BW1 - ntw$friendship_imp$BW2  
B_tiesbroken[!is.na(B_tiesbroken) & B_tiesbroken == -1] <- 0 

C_tiesbroken <- ntw$friendship$CW1 - ntw$friendship_imp$CW2
C_tiesbroken[!is.na(C_tiesbroken) & C_tiesbroken == -1] <- 0

# (3) Stable ties
A_tiesstable <- array(NA,dim=c(nrow(A_tiescreated),ncol(A_tiescreated),2))
A_tiesstable[,,1] <- ntw$friendship$AW1
A_tiesstable[,,2] <- ntw$friendship_imp$AW2
A_tiesstable <- apply(A_tiesstable,c(1,2),min)  
  
B_tiesstable <- array(NA,dim=c(nrow(B_tiescreated),ncol(B_tiescreated),2))
B_tiesstable[,,1] <- ntw$friendship$BW1
B_tiesstable[,,2] <- ntw$friendship_imp$BW2
B_tiesstable <- apply(B_tiesstable,c(1,2),min)  

C_tiesstable <- array(NA,dim=c(nrow(C_tiescreated),ncol(C_tiescreated),2))
C_tiesstable[,,1] <- ntw$friendship$CW1
C_tiesstable[,,2] <- ntw$friendship_imp$CW2
C_tiesstable <- apply(C_tiesstable,c(1,2),min)  

# Use zeroes instead of NAs in the predictors
A_tiescreated[is.na(A_tiescreated)] <- 0
A_tiesbroken[is.na(A_tiesbroken)] <- 0
A_tiesstable[is.na(A_tiesstable)] <- 0

B_tiescreated[is.na(B_tiescreated)] <- 0
B_tiesbroken[is.na(B_tiesbroken)] <- 0
B_tiesstable[is.na(B_tiesstable)] <- 0

C_tiescreated[is.na(C_tiescreated)] <- 0
C_tiesbroken[is.na(C_tiesbroken)] <- 0
C_tiesstable[is.na(C_tiesstable)] <- 0

# QAP regressions
set.seed(0708) # random seed
A_qap <- netlm(ntw$communication$A,list(A_tiescreated,A_tiesbroken,A_tiesstable),nullhyp='qap',reps=5000)
B_qap <- netlm(ntw$communication$B,list(B_tiescreated,B_tiesbroken,B_tiesstable),nullhyp='qap',reps=5000)
C_qap <- netlm(ntw$communication$C,list(C_tiescreated,C_tiesbroken,C_tiesstable),nullhyp='qap',reps=5000)

# Removal of unnecesary objects
rm(A_tiescreated);rm(A_tiesbroken);rm(A_tiesstable)
rm(B_tiescreated);rm(B_tiesbroken);rm(B_tiesstable)
rm(C_tiescreated);rm(C_tiesbroken);rm(C_tiesstable)

########################################################################################################################

# Tabulation of results
results <- data.frame(effect=c('Constant','Tie creation','Tie destruction','Tie maintenance'))
results$A_est <- A_qap$coefficients
results$A_p <- A_qap$pgreqabs
results$B_est <- B_qap$coefficients
results$B_p <- B_qap$pgreqabs
results$C_est <- C_qap$coefficients
results$C_p <- C_qap$pgreqabs

# Adjust p values with Benjamini's & Hochberg's (BH) method
results$A_p <- p.adjust(results$A_p,method = 'BH')
results$B_p <- p.adjust(results$B_p,method = 'BH')
results$C_p <- p.adjust(results$C_p,method = 'BH')

sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*','')))
}

results$A_s <- sig(results$A_p)
results$B_s <- sig(results$B_p)
results$C_s <- sig(results$C_p)

write.csv(results[,c('effect','A_est','A_p','A_s','B_est','B_p','B_s','C_est','C_p','C_s')],'QAPs.csv',row.names=FALSE)

########################################################################################################################

# Save image
save.image('QAPs.RData')
