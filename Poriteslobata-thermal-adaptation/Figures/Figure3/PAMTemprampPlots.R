setwd("~/MEGAsync/Manuscripts/Barshis_SanFranPoloLocalAdaptAmSam/JEBSubmission/Poriteslobata-Thermal-Adaptation/Figures/Figure3")

library(Hmisc)
PAM=read.csv("TempRampFullyEditedForR.csv")

pammean=tapply(PAM$Yield,list(PAM$Group,PAM$Day), mean)

pammean

pamSD=tapply(PAM$Yield,list(PAM$Group,PAM$Day), sd)

pamN=tapply(PAM$Yield,list(PAM$Group,PAM$Day), length)
pamN

pamSE=pamSD/sqrt(pamN)
pamSE

pammean

#adding temperature data
pamtemp=read.delim("Tempramp Temp Data for R.txt")
xtime=(1:463)
scaledxtime=(xtime-85)/93
scaledxtime
scaledtemp=(pamtemp$Tank.2-27)/21.667 #full spread of temp range 0-13 scaled by pam axis 0.6 (13/0.6)

# for all days
pdf(file="Fig3_AllDaysPamTempRampwide2v2_SD.pdf", 10, 7)
par(mar=c(7,6,2,5))
group=1:2
days=1:5
sym=c(1,19)
x.pos<-c(-0.1,0.1,0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1)
errbar(x.pos, pammean[group,days], pammean[group,days]+pamSD[group,days], pammean[group,days]-pamSD[group,days],  ylim=c(-0.025,0.6), xlim=c(-0.25, 4.25), pch=sym, type="b", cex=2.5, ylab="Effective Quantum Yield (Fv/Fm)", xlab="Days" , xaxt='n')
axis(1,at=c(0,1,2,3,4), labels=c("Aug-01", "Aug-02", "Aug-03", "Aug-04", "Aug-05"))
group=3:4
sym=c(0,15)
errbar(x.pos, pammean[group,days], pammean[group,days]+pamSD[group,days], pammean[group,days]-pamSD[group,days],  add=TRUE, pch=sym, type="b", cex=2.5, xaxt='n')
group=5:6
sym=c(2,17)
errbar(x.pos, pammean[group,days], pammean[group,days]+pamSD[group,days], pammean[group,days]-pamSD[group,days],  add=TRUE, pch=sym, type="b", cex=2.5, xaxt='n')

lines(scaledxtime, (pamtemp$Tank.2-27)/21.667)
axis(4,at=c(0,0.1,0.2,0.3,0.4,0.5,0.6),labels=sprintf("%.1f",seq(27,40, length.out=7)))
text(4.85,0.25,"Temperature °C", srt=-90, xpd=T)
legend("topright",c("HV","MV","forereef","stable","variable"), pch=c(0,2,1,0,15), pt.cex=1.5, bty="n")
text(-0.25,c(0.335,0.3,0.265),c("O**","D***","OxD***"), cex=1.25, adj=c(0,0))
text(2:3,-0.025,"O*",cex=1.25)
dev.off()

