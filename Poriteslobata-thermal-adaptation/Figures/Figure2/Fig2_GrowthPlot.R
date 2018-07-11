setwd("~/MEGAsync/Manuscripts/Barshis_SanFranPoloLocalAdaptAmSam/JEBSubmission/Poriteslobata-Thermal-Adaptation/Figures/Figure2")

library(Hmisc)

growth <- read.delim("Caliper_measurements_v2.txt", header=TRUE, sep='\t')

growth <- growth[!is.na(growth$Growth),]

###Boing
boing<-aggregate(Growth ~ origin + acclimation.tank + source.colony, data=growth, mean)

boingmean<-tapply(boing$Growth, list(boing$origin, boing$acclimation.tank), mean)
boingmean

boingSD<-tapply(boing$Growth, list(boing$origin, boing$acclimation.tank), sd)
boingSD

boingN=tapply(boing$Growth, list(boing$origin, boing$acclimation.tank), length)
boingN

boingSE=boingSD/sqrt(boingN)
boingSE

###growth rxn norm
pdf(file='Fig2_Growthpostacclimation_wlegend.pdf')
par(mar=c(7,6,2,4))
group=2
tank=2:1
sym=0
errbar(0:1, boingmean[group,tank], boingmean[group,tank]+boingSD[group,tank], boingmean[group,tank]-boingSD[group,tank], ylim=c(4, 14.5), xlim=c(-0.25,1.25), pch=sym, type='b', cex=4, ylab='New Growth (mm)', xlab='Acclimation Treatment', xaxt='n')
axis(1,at=c(0,1), labels=c("Stable", "Variable"))
group=3
sym=2
errbar(0:1, boingmean[group,tank], boingmean[group,tank]+boingSD[group,tank], boingmean[group,tank]-boingSD[group,tank],add=TRUE, pch=sym, type='b', cex=4, xaxt='n')
group=1
sym=1
errbar(0:1, boingmean[group,tank], boingmean[group,tank]+boingSD[group,tank], boingmean[group,tank]-boingSD[group,tank],add=TRUE, pch=sym, type='b', cex=4, xaxt='n')
legend("bottomright",c("HV", "MV", "forereef"), pch=c(0,2,1), pt.cex=1.5, bty='n')
#title(main='Growth Differences after 36 Days of Acclimation')
text(0.5,4.75,"O*", cex=1.5)
dev.off()

