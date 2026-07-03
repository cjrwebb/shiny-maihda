#FOR FULL PREDICTIONS

#assume predictions have already been calculated by predictInterval, merged
#into main dataset, and aggregated down to strata level, as in tutorial.

#remove unnecesary variables from dataframe
stratum_level2 <- subset(stratum_level, select=-c(strataN, HbA1c))

#calculate rank
stratum_level2 <- stratum_level2 %>%
  mutate(rank=rank(m1Bmfit))

#make plot (equivalent to fig 2a of tutorial paper)
ggplot(stratum_level2, aes(y=m1Bmfit, x=rank)) +
  geom_point() +
  geom_pointrange(aes(ymin=m1Bmlwr, ymax=m1Bmupr)) +
  ylab("Predicted HbA1c, Model 1B") +
  xlab("Stratum Rank") + 
  theme_bw()

#export CSV file
write.csv(stratum_level2, "fullprediction.csv")

#FOR RESIDUALS ONLY

#calculate residuals as in tutorial
m1Bu <- REsim(model1B)

#make plot
plotREsim(m1Bu) 

#merge in strata-defining variables
m1Bu <- merge(stratum_level2, m1Bu, by.x="stratum", by.y="groupID")

#note that this dataframe contains both full prediction and residual prediciton
#but I'm separating them for now to keep life simple for us
#so removing the full predictions now (along with other unneeded variables)

#remove unnecessary variables from data frame
m1Bu <- subset(m1Bu, select=-c(m1Bmfit, m1Bmupr, m1Bmlwr, groupFctr, term, median))

#now the plotREsim code won't work. But the equivalent to do it manually would be:


#calculate rank
m1Bu <- m1Bu %>%
  mutate(rank=rank(mean))

#make the plot
ggplot(m1Bu, aes(x=rank, y=mean)) +
  geom_point(size=3) +
  geom_pointrange(aes(ymin=mean-(1.96*sd), ymax=mean+(1.96*sd))) + 
  geom_hline(yintercept=0, color="red", linewidth=1)+
  xlab("Stratum Rank") +
  ylab("Predicted stratum Random Effect in HbA1c (mmol/mol)")+
  theme_bw()
#note that this is the same as above, except in the calculation of the CIs

write.csv(m1Bu, "multiplicative.csv")

