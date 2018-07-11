library(lsmeans)
host<-read.delim("San_Francisco_hostTempramp.txt", header=T)
head(host)

host$SColLet<-letters[host$SourceColony]

#Field Baseline
hostfield<-host[host$Day=="FBS",]
aggregate(Hsp70 ~ Origin, data=hostfield, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70~Group, data=hostfield)
n1<-aov(Hsp70 ~ Origin, hostfield)
summary(n1)

#Acclimation Baseline
hostbaseline<-host[host$Day=="BS",]
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=hostbaseline, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70~Group, data=hostbaseline)
n1<-aov(Hsp70 ~ Origin*AcclimationTank + Error(SColLet), hostbaseline)
summary(n1)
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))

#FBS vs BS
hostbases<-host[1:44,]
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=hostbases, FUN=function(x) shapiro.test(x)$p.value)
bartlett.test(Hsp70 ~ Group, data=hostbases)

n1<-aov(Hsp70 ~ Origin*AcclimationTank + Error(SColLet), hostbases)
summary(n1)
print(lsmeans(n1, list(pairwise ~ AcclimationTank)), adjust = c("tukey"))

#Ramp
hostramp<-host[45:nrow(host),]
aggregate(Hsp70 ~ Origin + AcclimationTank + Day, data=hostramp, FUN=function(x) shapiro.test(x)$p.value)
for(i in names(table(hostramp$Day))[2:6]){
	print(paste0("Day",i))
	print(bartlett.test(Hsp70 ~ Group, data=hostramp[hostramp$Day==i,]))
}

n1<-aov(Hsp70 ~ Origin*AcclimationTank*Day + Error(SColLet), hostramp)
summary(n1)
print(lsmeans(n1, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Day)), adjust = c("tukey"))
print(lsmeans(n1, list(pairwise ~ Origin | Day)), adjust = c("tukey"))
