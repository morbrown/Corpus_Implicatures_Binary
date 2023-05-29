# created by jdegen
# code for generating the figures and analysis reported in 
# Degen, J. (2015). Investigating the distribution of _some_ (but not _all_) implicatures using corpora and web-based methods. Semantics & Pragmatics.

library(Hmisc)
library(gridExtra)
library(ggplot2)
library(bootstrap)
library(MuMIn)
library(lme4)
library(dplyr)
library(tidyr)
library(tidyverse)
theme_set(theme_bw())

# set your path here
setwd("/Users/morbrown/code/corpus_implicatures_binary")
source("Rscripts/helpers.R")

# read data
raw_data = read.table("data_binary/corpus_implicatures_binary_pilot-trials.tsv",sep="\t",header=T,quote="")

raw_data_updated = raw_data %>%
  filter(!tgrep_id=="bot_check") %>%
  filter(!tgrep_id=="example1") %>%
  filter(!tgrep_id=="example2")

class(raw_data_updated$means_same)

raw_data_updated$means_same = as.numeric(as.character(raw_data_updated$means_same)) 

class(raw_data_updated$means_same)

#Reading in data from original likert scale experiment, in which each item is 
#coded as partitive or non-partitive. Then isolating only the tgrep id and 
#partitive info for each item, and only keeping those items which appeared
#in the pilot experiment

judith_data = read.table("data_binary/some_database_copy.csv", sep="\t",header=T,quote="")

judith_data_partitive = judith_data %>%
  select(Item, Partitive) %>%
  distinct() %>%
  mutate(tgrep_id = Item) %>%
  select(tgrep_id, Partitive) %>%
  filter(tgrep_id %in% raw_data_updated$tgrep_id)

#Merging the partitive data with my data

data_partitive = merge(raw_data_updated, judith_data_partitive, by = "tgrep_id")

agr = aggregate(means_same ~ tgrep_id, data=raw_data_updated, FUN=mean)

ggplot(agr,aes(x=means_same)) +
  geom_histogram() +
  scale_x_continuous(name="Mean by-item implicature strength rating",breaks=seq(0,1,by=0.2)) 

# Figure 10: Distribution of simulated mean by-item ratings.
likert = c(1,2,3,4,5,6,7)
means = data.frame(Mean=replicate(1363,mean(sample(likert,size=10,replace=T))))
summary(means$Mean)
sd(means$Mean)
ggplot(means,aes(x=Mean)) +
  geom_histogram() +
  scale_y_continuous(limits=c(0,180)) +  
  scale_x_continuous(limits=c(1,7),breaks=seq(1,7,by=1),name=c("Mean by-item rating (simulated)"))


# Figure 2: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for non-partitive and partitive some-NPs.
agr_partitive = aggregate(means_same ~ tgrep_id + Partitive, data=data_partitive,FUN=mean)
agrr_partitive = aggregate(means_same ~ Partitive, data=agr_partitive, FUN=mean)
agrr_partitive$CILow = aggregate(means_same ~ Partitive, data=agr_partitive, FUN=ci.low)$means_same
agrr_partitive$CIHigh = aggregate(means_same ~ Partitive, data=agr_partitive, FUN=ci.high)$means_same
agrr_partitive$YMin = agrr_partitive$means_same - agrr_partitive$CILow
agrr_partitive$YMax = agrr_partitive$means_same + agrr_partitive$CIHigh
agrr_partitive$Freq = as.data.frame(table(agr_partitive$Partitive))$Freq

pm = ggplot(agrr_partitive,aes(x=Partitive,y=means_same,fill=Partitive)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=0,ymax=1),width=.2) +
  scale_x_discrete(name="",breaks=levels(agrr_partitive$Partitive),labels=c("non-partitive","partitive")) +
  scale_fill_grey(start=0.7,end=0.35,name="") +  
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)

pm

agr = aggregate(Rating ~ Item + Partitive, data=d, FUN=mean)
pd = ggplot(agr,aes(x=Rating,fill=Partitive)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.35,breaks=levels(agr$Partitive),labels=c("non-partitive","partitive")) +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)


# Figure 3: Distribution of mean by-item determiner strength ratings overall (left) and conditioned on whether or not the some-NP was overtly partitive (right). Higher ratings indicate weaker determiner uses.
agr = aggregate(StrengthSome ~ Item, data=d, FUN=mean)
p = ggplot(agr,aes(x=StrengthSome)) +
  geom_histogram(position="dodge") +
  geom_density(alpha=.3) +
  scale_x_continuous(name="Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker"))  
p

agr = aggregate(StrengthSome ~ Item + Partitive, data=d, FUN=mean)
pp = ggplot(agr,aes(x=StrengthSome,fill=Partitive)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.3,breaks=c("no","yes"),labels=c("non-partitive","partitive")) +
  scale_x_continuous(name="Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker")) 
pp

grid.arrange(p,pp,nrow=1)
            

# Figure 4: Mean by-item implicature rating as a function of decreasing determiner strength.
agr = aggregate(Rating ~ Item + StrengthSome, data=d, FUN=mean)
ggplot(agr,aes(x=StrengthSome,y=Rating)) +
  stat_sum(size=3,aes(alpha=..n..)) +
  scale_y_continuous(name="Mean implicature strength rating") +
  scale_x_continuous("Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker")) +
  geom_smooth(method="lm",color="black",size=1)  


# Figure 5: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for new, mediated, and old embedded NP referents.
agr = aggregate(Rating ~ Item + Mention, data=d, FUN=mean)
agrr = aggregate(Rating ~ Mention, data=agr, FUN=mean)
agrr$CILow = aggregate(Rating ~ Mention, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Mention, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Mention))$Freq
agrr$Mention = factor(x=as.character(agrr$Mention),levels=c("new","med","old"))

pm = ggplot(agrr,aes(x=Mention,y=Rating, fill=Mention)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_manual(values=c("gray60","gray85","gray30")) +  
  scale_x_discrete(name="",breaks=levels(agrr$Mention),labels=c("new","mediated","old")) +
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm

agr = aggregate(Rating ~ Item + Mention, data=d, FUN=mean)
agr$Mention = factor(x=as.character(agr$Mention),levels=c("new","med","old"))
pd = ggplot(agr,aes(x=Rating,fill=Mention)) +
  geom_histogram(position="dodge") +
  scale_fill_manual(values=c("gray60","gray85","gray30")) +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)


# Figure 6: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for other and subject some-NPs.

judith_data_subjecthood = judith_data %>%
  select(Item, Subjecthood) %>%
  distinct() %>%
  mutate(tgrep_id = Item) %>%
  select(tgrep_id, Subjecthood) %>%
  filter(tgrep_id %in% raw_data_updated$tgrep_id)

data_subjecthood = merge(raw_data_updated, judith_data_subjecthood, by = "tgrep_id")

agr_subj = aggregate(means_same ~ tgrep_id + Subjecthood, data=data_subjecthood, FUN=mean)
agrr_subj = aggregate(data=agr_subj, means_same ~ Subjecthood, FUN=mean)
agrr_subj$CILow = aggregate(means_same ~ Subjecthood, data=agr_subj, FUN=ci.low)$means_same
agrr_subj$CIHigh = aggregate(means_same ~ Subjecthood, data=agr_subj, FUN=ci.high)$means_same
agrr_subj$YMin = agrr_subj$means_same - agrr_subj$CILow
agrr_subj$YMax = agrr_subj$means_same + agrr_subj$CIHigh
agrr_subj$Freq = as.data.frame(table(agr_subj$Subjecthood))$Freq

pm_subj = ggplot(agrr_subj,aes(x=Subjecthood,y=means_same,fill=Subjecthood)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_grey(start=.9,end=.35) +
  scale_x_discrete(name="",breaks=levels(agrr_subj$Subjecthood),labels=c("other","subject")) +
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm_subj

agr = aggregate(Rating ~ Item + Subjecthood, data=d, FUN=mean)
pd = ggplot(agr,aes(x=Rating,fill=Subjecthood)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.35,breaks=levels(agr$Subjecthood),labels=c("other","subject"),name="Subjecthood") +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)


# Figure 7: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for modified and unmodified some-NPs.
agr = aggregate(Rating ~ Item + Modification, data=d, FUN=mean)
agrr = aggregate(data=agr, Rating ~ Modification, FUN=mean)
agrr$CILow = aggregate(Rating ~ Modification, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Modification, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Modification))$Freq

pm=ggplot(agrr,aes(x=Modification,y=Rating,fill=Modification)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_x_discrete(name="",breaks=levels(agrr$Modification),labels=c("modified","unmodified")) +
  scale_fill_grey(start=.7,end=.25) +
  scale_y_continuous("Mean implicature strength") +
  geom_text(y=0.5,aes(label=Freq),size=4)
pm

agr = aggregate(Rating ~ Item + Modification, data=d, FUN=mean)
pd = ggplot(agr,aes(x=Rating,fill=Modification)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.25,breaks=levels(agr$Modification),labels=c("modified","unmodified"),name="Modification") +
  scale_y_continuous(name="Number of cases") +
  scale_x_continuous("Mean by-item implicature strength",breaks=seq(1,7,by=1))
pd

grid.arrange(pm,pd,nrow=1)

# Figure 8: Mean implicature strength ratings by linguistic mention (old/new embedded NP referent), subjecthood (subject/other some-NP), and modification (modified/unmodified embedded NP).
d$redMention = as.factor(ifelse(d$Mention == "new","new","old"))
agr = aggregate(Rating ~ Item + Modification + Subjecthood + redMention,FUN=mean, data=d)
agrr = aggregate(data=agr, Rating ~ Modification + Subjecthood + redMention, FUN=mean)
agrr$CILow = aggregate(Rating ~ Modification + Subjecthood + redMention, data=agr, FUN=ci.low)$Rating
agrr$CIHigh = aggregate(Rating ~ Modification + Subjecthood + redMention, data=agr, FUN=ci.high)$Rating
agrr$YMin = agrr$Rating - agrr$CILow
agrr$YMax = agrr$Rating + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Modification,agr$Subjecthood,agr$redMention))$Freq
dodge = position_dodge(.9)

ggplot(agrr,aes(x=redMention,y=Rating,fill=Subjecthood)) +
  geom_bar(stat="identity",position=dodge,width=.9) +  
  geom_bar(stat="identity",color="black",position=dodge,width=.9,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_x_discrete(name="Linguistic mention") +
  scale_fill_manual(values=c("gray40","gray80"),name="Subjecthood") +
  scale_y_continuous("Mean implicature strength") +
  geom_text(aes(label=Freq,y=0.5),position=dodge,size=4) +
  facet_wrap(~Modification)


# Figure 9: Scatterplot of empirical versus predicted mean by-item strength ratings for basic model (left panel, only by-participant random intercepts), intermediate model (center panel, additionally fixed effects of interest), and final model (right panel, additionally by-item random intercepts and by-participant random slopes for fixed effects). 
d$logSentenceLength = log(d$SentenceLength)
centered = cbind(d, myCenter(d[,c("StrengthSome","logSentenceLength","Subjecthood","Modification","Partitive","redMention")]))

m.random = lmer(Rating ~  (1|workerid), data=centered)
summary(m.random)

m.fixed = lmer(Rating ~ cPartitive*cStrengthSome+credMention*cSubjecthood*cModification + clogSentenceLength + (1|workerid), data=centered)
summary(m.fixed)

anova(m.random,m.fixed)

m = lmer(Rating ~ cPartitive*cStrengthSome+credMention*cSubjecthood*cModification + clogSentenceLength + (1|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credMention|workerid) + (0 + cSubjecthood|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid) + (1|Item), data=centered)
msummary = summary(m)

coefs = as.data.frame(msummary$coefficients)
summary(coefs)

# create the model summary reported in Table 5, Appendix D
createLatexTableLinear(coefs,predictornames=c("Intercept","Partitive","Strength","Linguistic mention","Subjecthood","Modification","Sentence length","Partitive:Strength","Linguistic mention:Subjecthood","Linguistic mention:Modification","Subjecthood:Modification","Linguistic mention:Subjecthood:Modification"))

anova(m.fixed,m)

# BIC comparison
BIC(m.random) # bic: 58452.95
BIC(m.fixed) # bic: 55938
BIC(m) # bic: 54015.73

# R squared -- marginal: proportion of variance explained by the fixed factors alone: .14. conditional: proportion of variance explained by both the fixed and random factors: .46.
r.squaredGLMM(m.random)
r.squaredGLMM(m.fixed)
r.squaredGLMM(m)

centered$Predicted = fitted(m)
centered$PredictedFixed = fitted(m.fixed)
centered$PredictedRandom = fitted(m.random)

# plot predicted values
agr = aggregate(Rating ~ Item, FUN=mean, data=centered)
agr$Predicted = aggregate(Predicted ~ Item, FUN=mean, data=centered)$Predicted
agr$PredictedFixed = aggregate(PredictedFixed ~ Item, FUN=mean, data=centered)$PredictedFixed
agr$PredictedRandom = aggregate(PredictedRandom ~ Item, FUN=mean, data=centered)$PredictedRandom

cor(agr$Predicted, agr$Rating) # corr: .99
cor(agr$PredictedFixed, agr$Rating) # corr: .66
cor(agr$PredictedRandom, agr$Rating) # corr: .16

r = data.frame(X=c(6),Y=c(1.5),R=paste("r = ",gsub("0.",".",round(cor(agr$Predicted,agr$Rating),2)),sep=""))
p1=ggplot(agr, aes(x=Predicted,y=Rating)) +
  geom_point(size=1.5) +
  geom_smooth(method="lm",size=2) +
  scale_x_continuous(name="",limits=c(1.5,6.5)) +  
  scale_y_continuous(name="",limits=c(1,6.5))  +  
  geom_text(data=r,aes(x=X,y=Y,label=R))  
p1

r = data.frame(X=c(6),Y=c(1.5),R=paste("r = ",gsub("0.",".",round(cor(agr$PredictedFixed,agr$Rating),2)),sep=""))
p2=ggplot(agr, aes(x=PredictedFixed,y=Rating)) +
  geom_point(size=1.5) +
  geom_smooth(method="lm",size=2) +
  scale_x_continuous(name="",limits=c(1.5,6.5)) +  
  scale_y_continuous(name="",limits=c(1,6.5))  +  
  geom_text(data=r,aes(x=X,y=Y,label=R))
p2

r = data.frame(X=c(6),Y=c(1.5),R=paste("r = ",gsub("0.",".",round(cor(agr$PredictedRandom,agr$Rating),2)),sep=""))
p3=ggplot(agr, aes(x=PredictedRandom,y=Rating)) +
  geom_point(size=1.5) +
  geom_smooth(method="lm",size=2) +
  scale_x_continuous(name="",limits=c(1.5,6.5)) +  
  scale_y_continuous(name="",limits=c(1,6.5))  +  
  geom_text(data=r,aes(x=X,y=Y,label=R))
p3

grid.arrange(p3,p2,p1,nrow=1,sub=textGrob("Model-predicted by-item mean strength",gp=gpar(fontsize=17),vjust=0),left=textGrob("Empirical by-item mean strength",gp=gpar(fontsize=17),vjust=0.5,rot=90))



