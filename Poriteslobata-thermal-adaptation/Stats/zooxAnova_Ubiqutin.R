library(lsmeans)

zoox<-read.delim("San_Francisco_Zoox_Tempramp.txt", header=T)
head(zoox)

zoox$SColLet<-letters[zoox$SourceColony]
#Field Baseline
zooxfield<-zoox[zoox$Day=="FBS",]
aggregate(Ubiquitin ~ Origin, data=zooxfield, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin ~ Group, data=zooxfield)
fligner.test(Ubiquitin ~ Group, data=zooxfield)
n1<-aov(Ubiquitin ~ Origin, zooxfield)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))

#Acclimation Baseline
zooxbaseline<-zoox[zoox$Day=="BS",]
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=zooxbaseline, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin ~ Group, data=zooxbaseline)
fligner.test(Ubiquitin ~ Group, data=zooxbaseline)
n1<-aov(Ubiquitin ~ Origin*AcclimationTank + Error(SColLet), zooxbaseline)
summary(n1)

#FBS vs BS
zooxbases<-zoox[1:44,]
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=zooxbases, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin ~ Group, data=zooxbases)
fligner.test(Ubiquitin ~ Group, data=zooxbases)
n1<-aov(Ubiquitin ~ Origin*AcclimationTank + Error(SColLet), zooxbases)
summary(n1)

#TempRamp
zooxramp<-zoox[45:nrow(zoox),]
zooxramp$groupday<-paste(zooxramp$Group, zooxramp$Day, sep="-")
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=zooxramp, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin ~ groupday, data=zooxramp)
fligner.test(Ubiquitin ~ groupday, data=zooxramp)
n1<-aov(Ubiquitin ~ Origin*AcclimationTank*Day + Error(SColLet), zooxramp)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day | AcclimationTank | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Day | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | Day | AcclimationTank)), adjust = c("tukey"))
