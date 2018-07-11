library(lsmeans)
zoox<-read.delim("San_Francisco_Zoox_Tempramp.txt", header=T)
head(zoox)

####Hsp70
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=zoox, FUN=function(x) shapiro.test(x)$p.value)
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=zoox, mean)
zoox$SColLet<-letters[zoox$SourceColony]

#Field Baseline
zooxfield<-zoox[zoox$Day=="FBS",]
aggregate(Hsp70 ~ Origin, data=zooxfield, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70 ~ Group, data=zooxfield)
fligner.test(Hsp70 ~ Group, data=zooxfield)
n1<-aov(Hsp70 ~ Origin, zooxfield)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))

#Acclimation Baseline
zooxbaseline<-zoox[zoox$Day=="BS",]
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=zooxbaseline, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70 ~ Group, data=zooxbaseline)
fligner.test(Hsp70 ~ Group, data=zooxbaseline)
n1<-aov(Hsp70 ~ Origin*AcclimationTank + Error(SColLet), zooxbaseline)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | AcclimationTank)), adjust = c("tukey"))

#FBS vs BS
zooxbases<-zoox[1:44,]
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=zooxbases, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70 ~ Group, data=zooxbases)
fligner.test(Hsp70 ~ Group, data=zooxbases)
n1<-aov(Hsp70 ~ Origin*AcclimationTank + Error(SColLet), zooxbases)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | AcclimationTank)), adjust = c("tukey"))

#TempRamp
zooxramp<-zoox[45:nrow(zoox),]
zooxramp$groupday<-paste(zooxramp$Group, zooxramp$Day, sep="-")
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=zooxramp, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70 ~ groupday, data=zooxramp)
fligner.test(Hsp70 ~ groupday, data=zooxramp)
n1<-aov(Hsp70 ~ Origin*AcclimationTank*Day + Error(SColLet), zooxramp)
summary(n1)
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day)), adjust = c("tukey"))
#print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day | AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day | AcclimationTank | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Day | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | Day | AcclimationTank)), adjust = c("tukey"))
