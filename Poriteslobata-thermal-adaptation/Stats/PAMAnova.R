PAM<-read.csv("TempRampFullyEditedForR.csv")
PAM
aggregate(Yield ~ Origin + Acclimation.Tank + Day, data=PAM, FUN=function(x) shapiro.test(x)$p.value)
boxplot(Yield ~ Origin + Acclimation.Tank + Day + Source.Colony, data=PAM, las=2)

###Average source colony fragments
boingPAM<-aggregate(Yield ~ Origin + Acclimation.Tank + Source.Colony + Day, data=PAM, mean)
boingPAM$Source.Colony<-letters[boingPAM$Source.Colony]
boingPAM$Day<-factor(boingPAM$Day)
aggregate(Yield ~ Origin + Acclimation.Tank + Day, data=boingPAM, FUN=function(x) shapiro.test(x)$p.value)
boxplot(Yield ~ Origin + Acclimation.Tank + Day, data=boingPAM, las=2)
boingPAM$Group<-paste(boingPAM$Origin, boingPAM$Acclimation.Tank, sep="-")
for(i in names(table(boingPAM$Day))){
	print(paste0("Day",i))
	print(bartlett.test(Yield ~ Group, data=boingPAM[boingPAM$Day==i,]))
}
###Repeated Measures###
day.aov<-aov(Yield ~ Origin*Acclimation.Tank*Day + Error(Source.Colony), data=boingPAM)
summary(day.aov)
library(lsmeans)
print(lsmeans(day.aov, list(pairwise ~ Origin)), adjust = c("tukey"))
print(lsmeans(day.aov, list(pairwise ~ Day)), adjust = c("tukey"))
print(lsmeans(day.aov, list(pairwise ~ Origin | Day)), adjust = c("tukey"))

