growth<-read.delim("Caliper_measurements_v2.txt")
growth$fragment<-apply(combn(letters[1:26],3), 2, function(x) paste(x[1],x[2],x[3],sep=""))[1:nrow(growth)]
growth$source.colonyletr<-letters[growth$source.colony]
growth$source.colony<-factor(growth$source.colony)
head(growth)
aggregate(Growth ~ origin + acclimation.tank, data=growth, summary)
aggregate(Growth ~ origin + acclimation.tank, data=growth, sd)
aggregate(Growth ~ origin + acclimation.tank, data=growth, FUN= function(x) shapiro.test(x)$p.value)
qqnorm(growth$Growth)
qqline(growth$Growth)
bartlett.test(Growth ~ origin.acclimation, data=growth)
fligner.test(Growth ~ origin.acclimation, data=growth)
replications(Growth ~ origin*acclimation.tank*source.colony, growth)

###assessing whether average is an okay averaging technique
pdf("BoxplotGrowthMeans.pdf")
boxplot(Growth ~ source.colonyletr*acclimation.tank, data=growth)
dev.off()

boing<-aggregate(Growth ~ origin + acclimation.tank + source.colony, data=growth, mean)
aggregate(Growth ~ origin + acclimation.tank, data=boing, FUN= function(x) shapiro.test(x)$p.value)
head(growth)
head(boing)
qqnorm(boing$Growth)
qqline(boing$Growth)
replications(Growth ~ origin*acclimation.tank, boing)
boing$origin.acclimation<-paste(boing$origin,boing$acclimation, sep="-")
boing$source.colonyletr<-letters[boing$source.colony]
bartlett.test(Growth ~ origin.acclimation, data=boing)

n1<-aov(Growth ~ origin*acclimation.tank + Error(source.colonyletr), boing)
summary(n1)
library(lsmeans)
print(lsmeans(n1, list(pairwise ~ origin)), adjust = c("tukey"))
