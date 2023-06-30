# created by jdegen
# code for generating the figures and analysis reported in 
# Degen, J. (2015). Investigating the distribution of _some_ (but not _all_) implicatures using corpora and web-based methods. Semantics & Pragmatics.

library(tidyverse)
library(lme4)
library(gridExtra)
library(dplyr)
library(ggeffects)

theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# set your path here
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

# read data
raw_data = read_tsv("../data/corpus_implicatures_binary_pilot-trials.tsv",quote="")
test_data = read_tsv("../data/corpus_implicatures_binary_test-trials.tsv",quote="")
main_data = read_tsv("../data/corpus_implicatures_main-trials.tsv",quote="")
second_data = read_tsv("../data/corpus_implicatures_second_main-trials.tsv",quote="")

#cleaning each dataframe before rbinding together into one dataframe
raw_data = raw_data %>%
  filter(!tgrep_id=="bot_check") %>%
  filter(!tgrep_id=="example1") %>%
  filter(!tgrep_id=="example2")%>%
  separate(col="response", into=c("means_same", "sounds_weird"), sep=",")%>%
  mutate(response_numeric = case_when(means_same == "['Yes'" ~ 1, TRUE ~ 0))

test_data = test_data %>%
  filter(!tgrep_id=="bot_check") %>%
  filter(!tgrep_id=="example1") %>%
  filter(!tgrep_id=="example2")%>%
  separate(col="response", into=c("means_same", "sounds_weird"), sep=",")%>%
  mutate(response_numeric = case_when(means_same == "['Yes'" ~ 1, TRUE ~ 0))

main_data = main_data %>%
  filter(!tgrep_id=="bot_check") %>%
  filter(!tgrep_id=="example1") %>%
  filter(!tgrep_id=="example2")%>%
  separate(col="response", into=c("means_same", "sounds_weird"), sep=",")%>%
  mutate(response_numeric = case_when(means_same == "['Yes'" ~ 1, TRUE ~ 0))

second_data = second_data %>%
  filter(!tgrep_id=="bot_check") %>%
  filter(!tgrep_id=="example1") %>%
  filter(!tgrep_id=="example2")%>%
  separate(col="response", into=c("means_same", "sounds_weird"), sep=",")%>%
  mutate(response_numeric = case_when(means_same == "['Yes'" ~ 1, TRUE ~ 0))



total_data <- rbind(raw_data, test_data, main_data, second_data)


#Reading in data from original likert scale experiment, in which each item is 
#coded as partitive or non-partitive. Then isolating only the tgrep id and 
#partitive info for each item, and only keeping those items which appeared
#in the pilot experiment

judith_data = read_tsv("../data/some_database_copy.csv",quote="") %>% 
  rename(tgrep_id=Item) %>% 
  select(tgrep_id,Partitive,StrengthSome,Mention,Subjecthood,Modification,SentenceLength) %>% 
  distinct() %>% 
  mutate(Partitive=fct_recode(Partitive,"partitive"="yes","non-partitive"="no"),
         Mention=fct_recode(Mention,"mediated"="med")) %>% 
  mutate(Mention=fct_relevel(Mention,"new"))
nrow(judith_data)
view(judith_data)

#Merging the case information into the binary judgment dataset
d = total_data %>% 
  left_join(judith_data, by = "tgrep_id")
view(d)



# plot histogram of by-item proportions
props = d %>% 
  group_by(tgrep_id) %>% 
  summarize(Proportion = mean(response_numeric))

props

props_gradient = props %>%
  filter(!Proportion==0.00000000) %>%
  filter(!Proportion==1.00000000)


ggplot(props,aes(x=Proportion)) +
  geom_histogram() +
  ylab("Cases") +
  scale_x_continuous(name="By-item implicature rate",breaks=seq(0,1,by=0.1)) 
ggsave("../graphs/props_byitem.pdf",width=4,height=3)

# Figure 2: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for non-partitive and partitive some-NPs.
agr = d %>% 
  group_by(Partitive) %>% 
  summarize(Proportion = mean(response_numeric), CILow=ci.low(response_numeric), CIHigh=ci.high(response_numeric)) %>% 
  ungroup() %>% 
  mutate(YMin=Proportion-CILow,YMax=Proportion+CIHigh)

agr_id = d %>% 
  group_by(tgrep_id, Partitive) %>% 
  summarize(Proportion = mean(response_numeric)) 

pdf(file="../graphs/props_pbar_partitive.pdf",width=5,height=3)

p_bar = ggplot(agr,aes(x=Partitive,y=Proportion,fill=Partitive)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous("Mean implicature rate",limits=c(0,1)) 
p_bar

dev.off()

p_bar

contrasts(d$Partitive)

d = d %>%
  mutate(numPartitive = as.numeric(Partitive)) %>%
  mutate(cPartitive = numPartitive - mean(numPartitive))
summary(d)


pdf(file="../graphs/props_phist_partitive.pdf",width=5,height=3)

#Why was coord_flip() added/is it visually better for plot to be sideways?
p_hist = ggplot(agr_id, aes(x=Proportion,fill=Partitive)) +
  geom_histogram(alpha=.5,position="identity") +
  ylab("Cases") +
  #coord_flip() +
  scale_fill_manual(values=cbPalette) +
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
p_hist

#pdf(file="../graphs/props_partitive.pdf",width=5,height=3)
#grid.arrange(p_bar,p_hist,nrow=1)
dev.off()

# Figure 6: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for other and subject some-NPs.
agr = d %>% 
  group_by(Subjecthood) %>% 
  summarize(Proportion = mean(response_numeric), CILow=ci.low(response_numeric), CIHigh=ci.high(response_numeric)) %>% 
  ungroup() %>% 
  mutate(YMin=Proportion-CILow,YMax=Proportion+CIHigh)

agr_id = d %>% 
  group_by(tgrep_id, Subjecthood) %>% 
  summarize(Proportion = mean(response_numeric)) 

pdf(file="../graphs/props_pbar_subjecthood.pdf",width=5,height=3)

p_bar = ggplot(agr,aes(x=Subjecthood,y=Proportion,fill=Subjecthood)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous("Mean implicature rate",limits=c(0,1)) 
p_bar

dev.off()

p_hist = ggplot(agr_id, aes(x=Proportion,fill=Subjecthood)) +
  geom_histogram(alpha=.5,position="identity") +
  ylab("Cases") +
  #coord_flip() +
  scale_fill_manual(values=cbPalette) +
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
p_hist

pdf(file="../graphs/props_subjecthood.pdf",width=5,height=3)
grid.arrange(p_bar,p_hist,nrow=1)
dev.off()

class(d$Subjecthood)

d = d %>%
  mutate(Subjecthood = as.factor(Subjecthood)) %>%
  mutate(numSubjecthood = as.numeric(Subjecthood)) %>%
  mutate(cSubjecthood = numSubjecthood - mean(numSubjecthood))
summary(d)

class(d$numSubjecthood)


# Figure 5: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for new, mediated, and old embedded NP referents.
agr = d %>% 
  group_by(Mention) %>% 
  summarize(Proportion = mean(response_numeric), CILow=ci.low(response_numeric), CIHigh=ci.high(response_numeric)) %>% 
  ungroup() %>% 
  mutate(YMin=Proportion-CILow,YMax=Proportion+CIHigh)

agr_id = d %>% 
  group_by(tgrep_id, Mention) %>% 
  summarize(Proportion = mean(response_numeric)) 

pdf(file="../graphs/props_pbar_mention.pdf",width=5,height=3)

p_bar = ggplot(agr,aes(x=Mention,y=Proportion,fill=Mention)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous("Mean implicature rate",limits=c(0,1))
p_bar

dev.off()

p_hist = ggplot(agr_id, aes(x=Proportion,fill=Mention)) +
  geom_histogram(alpha=.5,position="identity") +
  ylab("Cases") +
  #coord_flip() +
  scale_fill_manual(values=cbPalette) +
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
p_hist

grid.arrange(p_bar,p_hist,nrow=1)

pdf(file="../graphs/props_mention.pdf",width=5,height=3)
grid.arrange(p_bar,p_hist,nrow=1)
dev.off()

class(d$Mention)


# Figure 7: Mean implicature strength ratings (left) and distribution of mean by-item ratings (right) for modified and unmodified some-NPs.
agr = d %>% 
  group_by(Modification) %>% 
  summarize(Proportion = mean(response_numeric), CILow=ci.low(response_numeric), CIHigh=ci.high(response_numeric)) %>% 
  ungroup() %>% 
  mutate(YMin=Proportion-CILow,YMax=Proportion+CIHigh)

total_yes = sum(d$response_numeric)

total_ratings = nrow(d)

total_ratings

total_yes/total_ratings

agr_id = d %>% 
  group_by(tgrep_id, Modification) %>% 
  summarize(Proportion = mean(response_numeric)) 

pdf(file="../graphs/props_pbar_modification.pdf",width=5,height=3)

p_bar = ggplot(agr,aes(x=Modification,y=Proportion,fill=Modification)) +
  geom_bar(stat="identity",color="black",width=.5,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2) +
  scale_fill_manual(values=cbPalette) +
  scale_y_continuous("Mean implicature rate",limits=c(0,1))
p_bar

dev.off()

#Important!! There are three colors instead of two on this histogram because
#The two colors are mixed where they overlap
p_hist = ggplot(agr_id, aes(x=Proportion,fill=Modification)) +
  geom_histogram(alpha=.5,position="identity") +
  ylab("Cases") +
  #coord_flip() +
  scale_fill_manual(values=cbPalette) +
  theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
p_hist

pdf(file="../graphs/props_modification.pdf",width=5,height=3)
grid.arrange(p_bar,p_hist,nrow=1)
dev.off()

class(d$Modification)

d = d %>%
  mutate(Modification = as.factor(Modification))%>%
  mutate(numModification = as.numeric(Modification))

class(d$numModification)

# JD UPDATED FILE UP TO THIS POINT


# Figure 3: Distribution of mean by-item determiner strength ratings overall (left) and conditioned on whether or not the some-NP was overtly partitive (right). Higher ratings indicate weaker determiner uses.
agr = aggregate(StrengthSome ~ tgrep_id, data=d, FUN=mean)

p = ggplot(agr,aes(x=StrengthSome)) +
  geom_histogram(position="dodge") +
  geom_density(alpha=.3) +
  scale_x_continuous(name="Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker"))  
p

agr = aggregate(StrengthSome ~ tgrep_id + Partitive, data=d, FUN=mean)
pp = ggplot(agr,aes(x=StrengthSome,fill=Partitive)) +
  geom_histogram(position="dodge") +
  scale_fill_grey(start=0.7,end=0.3,breaks=c("no","yes"),labels=c("non-partitive","partitive")) +
  scale_x_continuous(name="Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker")) 
pp

grid.arrange(p,pp,nrow=1)


# Figure 4: Mean by-item implicature rating as a function of decreasing determiner strength.
agr = aggregate(response_numeric ~ tgrep_id + StrengthSome, data=d, FUN=mean)
fundetstrength = ggplot(agr,aes(x=StrengthSome,y=response_numeric)) +
  stat_sum(size=3,aes(alpha=..n..)) +
  scale_y_continuous(name="Mean implicature strength rating") +
  scale_x_continuous("Decreasing determiner strength",breaks=c(3,4,5,6,7),labels=c("3\n stronger","4","5","6","7\nweaker")) +
  geom_smooth(method="lm",color="black",size=1)  

fundetstrength

pdf(file="../graphs/fun_detstrength.pdf",width=5,height=3)
grid.arrange(fundetstrength, nrow=1)
dev.off()

class(d$StrengthSome)

d = d %>%
  mutate(cStrengthSome = StrengthSome - mean(StrengthSome))
summary(d)




# Figure 8: Mean implicature strength ratings by linguistic mention (old/new embedded NP referent), subjecthood (subject/other some-NP), and modification (modified/unmodified embedded NP).
d$redMention = as.factor(ifelse(d$Mention == "new","new","old"))
agr = aggregate(response_numeric ~ tgrep_id + Modification + Subjecthood + redMention,FUN=mean, data=d)
agrr = aggregate(data=agr, response_numeric ~ Modification + Subjecthood + redMention, FUN=mean)
agrr$CILow = aggregate(response_numeric ~ Modification + Subjecthood + redMention, data=agr, FUN=ci.low)$response_numeric
agrr$CIHigh = aggregate(response_numeric ~ Modification + Subjecthood + redMention, data=agr, FUN=ci.high)$response_numeric
agrr$YMin = agrr$response_numeric - agrr$CILow
agrr$YMax = agrr$response_numeric + agrr$CIHigh
agrr$Freq = as.data.frame(table(agr$Modification,agr$Subjecthood,agr$redMention))$Freq
dodge = position_dodge(.9)

ggplot(agrr,aes(x=redMention,y=response_numeric,fill=Subjecthood)) +
  geom_bar(stat="identity",position=dodge,width=.9) +  
  geom_bar(stat="identity",color="black",position=dodge,width=.9,show_guide=F) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_x_discrete(name="Linguistic mention") +
  scale_fill_manual(values=c("gray40","gray80"),name="Subjecthood") +
  scale_y_continuous("Mean implicature strength") +
  geom_text(aes(label=Freq,y=0.5),position=dodge,size=4) +
  facet_wrap(~Modification)

d = d %>%
  mutate(numredMention = as.numeric(redMention))


# Figure 9: Scatterplot of empirical versus predicted mean by-item strength ratings for basic model (left panel, only by-participant random intercepts), intermediate model (center panel, additionally fixed effects of interest), and final model (right panel, additionally by-item random intercepts and by-participant random slopes for fixed effects). 
d$logSentenceLength = log(d$SentenceLength)

#centereddata = cbind(d, scale(d[,c("StrengthSome","logSentenceLength","numSubjecthood","numModification","numPartitive","numredMention")], scale = FALSE))

#centered = data.frame(scale(d[,c("StrengthSome","logSentenceLength","numSubjecthood","numModification","numPartitive","numredMention")], scale = FALSE))

centered = d %>%
  mutate(numStrengthSome = c(scale(StrengthSome, scale = FALSE)))%>%
  mutate(numlogSentenceLength = c(scale(logSentenceLength, scale = FALSE)))%>%
  mutate(numSubjecthood = c(scale(numSubjecthood, scale = FALSE)))%>%
  mutate(numModification = c(scale(numModification, scale = FALSE)))%>%
  mutate(numPartitive = c(scale(numPartitive, scale = FALSE)))%>%
  mutate(numredMention = c(scale(numredMention, scale = FALSE)))

# JD WROTE THIS:
d = d %>% 
  mutate_at(c('Subjecthood', 'Partitive', 'Modification', 'redMention','means_same','tgrep_id','workerid'), as.factor)

contrasts(d$Subjecthood)
contrasts(d$Partitive)
contrasts(d$Modification)
contrasts(d$redMention)
contrasts(d$means_same)

centered = d %>%
  mutate(cStrengthSome = StrengthSome - mean(StrengthSome)) %>%
  mutate(clogSentenceLength = logSentenceLength - mean(logSentenceLength)) %>%
  mutate(cSubjecthood = as.numeric(Subjecthood) -mean(as.numeric(Subjecthood))) %>%
  mutate(cModification = as.numeric(Modification) - mean(as.numeric(Modification))) %>%
  mutate(cPartitive = as.numeric(Partitive) - mean(as.numeric(Partitive))) %>% 
  mutate(credMention = as.numeric(redMention) - mean(as.numeric(redMention))) 


# model with only random by-participant and by-item intercepts
m.simple = glmer(response_numeric ~ cPartitive*cStrengthSome+credMention*cSubjecthood*cModification + clogSentenceLength + (1|workerid) + (1|tgrep_id), 
                 data=centered, 
                 family="binomial",
                 control=glmerControl(optimizer="bobyqa",
                                      optCtrl=list(maxfun=2e5)))
summary(m.simple)


# model with more complex RE structure
m = glmer(means_same ~ cPartitive*cStrengthSome+credMention*cSubjecthood*cModification + clogSentenceLength + (1|workerid) + (0 + cPartitive|workerid) + (0 + cStrengthSome|workerid) + (0 + credMention|workerid) + (0 + cSubjecthood|workerid) + (0+cModification|workerid) + (0 + cPartitive:cStrengthSome|workerid) + (1|tgrep_id), 
          data=centered, 
          family="binomial",
          control=glmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
summary(m)

# END JD CODE


m.random = lmer(means_same ~  (1|workerid), data=centered)
summary(m.random)

m.random.logistic = glmer(means_same ~  (1|workerid), data=centered, family="binomial")
summary(m.random.logistic)

m.fixed = glmer(means_same ~ numPartitive*numStrengthSome+numredMention*numSubjecthood*numModification + numlogSentenceLength + (1|workerid), data=centered, family="binomial")
summary(m.fixed)

m.fixed.nointercept = glm(means_same ~ numPartitive*numStrengthSome+numredMention*numSubjecthood*numModification + numlogSentenceLength, data=centered, family="binomial")
m.fixed.nointerceptsummary = summary(m.fixed.nointercept)

contrasts(centered$numModification)

contrasts(centered$numPartitive)

ggpredict(m.fixed.nointercept, terms = c("numSubjecthood", "numModification"))%>%
plot()

ggpredict(m.fixed.nointercept, terms = c("numPartitive", "numStrengthSome"))%>%
  plot()

anova(m.random,m.fixed)

m = glmer(means_same ~ numPartitive*numStrengthSome+numredMention*numSubjecthood*numModification + numlogSentenceLength + (1|workerid) + (0 + numPartitive|workerid) + (0 + numStrengthSome|workerid) + (0 + numredMention|workerid) + (0 + numSubjecthood|workerid) + (0+numModification|workerid) + (0 + numPartitive:numStrengthSome|workerid) + (1|tgrep_id), data=centered, family="binomial")
msummary = summary(m)

coefs = as.data.frame(msummary$coefficients)
summary(coefs)

coefs.fixed.nointercept = as.data.frame(m.fixed.nointerceptsummary$coefficients)
summary(coefs.fixed.nointercept)

# create the model summary reported in Table 5, Appendix D
createLatexTableLinear(coefs,predictornames=c("Intercept","Partitive","Strength","Linguistic mention","Subjecthood","Modification","Sentence length","Partitive:Strength","Linguistic mention:Subjecthood","Linguistic mention:Modification","Subjecthood:Modification","Linguistic mention:Subjecthood:Modification"))

createLatexTableLinear(coefs.fixed.nointercept,predictornames=c("Intercept","Partitive","Strength","Linguistic mention","Subjecthood","Modification","Sentence length","Partitive:Strength","Linguistic mention:Subjecthood","Linguistic mention:Modification","Subjecthood:Modification","Linguistic mention:Subjecthood:Modification"))

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

#Histogram of completion times

completion_times_data = read.table("data_binary/corpus_implicatures_binary_pilot-time_in_minutes.csv",sep=",",header=T,quote="")

#This histogram isn't very helpful, I can try to improve it later today
completion_hist = ggplot(completion_times_data,aes(x=time_in_minutes)) +
  geom_histogram() +
  scale_x_continuous(name="Completion time",breaks=seq(0,11,by=5)) 

completion_hist

median(completion_times_data$time_in_minutes)




