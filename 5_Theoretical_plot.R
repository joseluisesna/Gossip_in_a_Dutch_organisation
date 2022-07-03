########################################################################################################################
## THE EFFECTS OF GOSSIP ON FRIENDSHIP IN A DUTCH CHILDCARE ORGANISATION
## Theoretical plot
## R script written by Jose Luis Estevez (Masaryk University & Linkoping University)
## Date: Jul 1st, 2022
########################################################################################################################

# R PACKAGES REQUIRED
library(ggplot2)

rm(list=ls())

x <- c(seq(0,1.4,by=.14),c(0,0,.8,0.88,.93,.96,.975,.985,.995,.998,1))
data <- as.data.frame(x)
data$senders <- c(seq(0,10,by=1),seq(0,10,by=1))
data$`Implicit model` <- c(rep('Reinforcement',11),rep('Threshold',11))

jpeg(filename='theory model.jpeg',width=7,height=4,units='in',res=500)
ggplot(data=data[data$senders <= 7,])+
  geom_line(aes(x=senders,y=x,color=`Implicit model`)) +
  geom_point(aes(x=senders,y=x,color=`Implicit model`),size=3.5,alpha=.65) +
  scale_colour_manual(values = c('dodgerblue','firebrick2'))+
  theme_classic() +
  scale_x_continuous(breaks = 0:7) +
  xlab('Number of gossip senders') + ylab('Probability that gossip affects friendship choices') +
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  theme(legend.position="top", legend.justification="center")
dev.off()