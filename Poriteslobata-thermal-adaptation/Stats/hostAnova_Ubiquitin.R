library(lsmeans)
host<-read.delim("San_Francisco_hostTempramp.txt", header=T)
head(host)

####Ubiquitin
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=host, FUN=function(x) shapiro.test(x)$p.value)
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=host, mean)

#Field Baseline
hostfield<-host[host$Day=="FBS",]
aggregate(Ubiquitin ~ Origin, data=hostfield, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin~Group, data=hostfield)
fligner.test(Ubiquitin~Group, data=hostfield)
n1<-aov(Ubiquitin ~ Origin, hostfield)
summary(n1)

#Acclimation Baseline
hostbaseline<-host[host$Day=="BS",]
hostbaseline$SColLet<-letters[hostbaseline$SourceColony]
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=hostbaseline, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin~Group, data=hostbaseline)
fligner.test(Ubiquitin~Group, data=hostbaseline)

n1<-aov(Ubiquitin ~ Origin*AcclimationTank + Error(SColLet), hostbaseline)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Origin)), adjust = c("tukey"))

#FBS vs BS
hostbases<-host[1:44,]
hostbases$SColLet<-letters[hostbases$SourceColony]
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=hostbases, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Ubiquitin ~ Group, data=hostbases)
fligner.test(Ubiquitin ~ Group, data=hostbases)

n1<-aov(Ubiquitin ~ Origin*AcclimationTank + Error(SColLet), hostbases)
summary(n1)
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank|Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin|AcclimationTank)), adjust = c("tukey"))

#Ramp
hostramp<-host[45:nrow(host),]
hostramp$SColLet<-letters[hostramp$SourceColony]
aggregate(Ubiquitin ~ Origin + AcclimationTank + Day, data=hostramp, FUN=function(x) shapiro.test(x)$p.value)
for(i in names(table(hostramp$Day))[2:6]){
	print(paste0("Day",i))
	print(bartlett.test(Ubiquitin ~ Group, data=hostramp[hostramp$Day==i,]))
}

for(i in names(table(hostramp$Day))[2:6]){
	print(paste0("Day",i))
	print(fligner.test(Ubiquitin ~ Group, data=hostramp[hostramp$Day==i,]))
}

n1<-aov(Ubiquitin ~ Origin*AcclimationTank*Day + Error(SColLet), hostramp)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ AcclimationTank | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day | Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day| Origin | AcclimationTank)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day| AcclimationTank | Origin)), adjust = c("tukey"))
