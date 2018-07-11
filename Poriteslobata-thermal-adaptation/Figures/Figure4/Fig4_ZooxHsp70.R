library(Hmisc)

zoox<-read.delim("San_Francisco_Zoox_Tempramp.txt", header=T)
head(zoox)

hsp70mean=tapply(zoox$Hsp70,list(zoox$Group,zoox$Day), mean)

hsp70mean

hsp70SD=tapply(zoox$Hsp70,list(zoox$Group,zoox$Day), sd)

#######1 panel #######
pdf(file="Fig4_AllDaysZooxHsp70_v3_SD.pdf", 10, 7)
par(mar=c(7,6,2,5))

errbar(0:6, hsp70mean[2,], type="n", ylim=c(-0.2,3), xlim=c(-0.25,6.5), xlab='', ylab=substitute(paste(italic('Symbiodinium'),' Hsp70 Levels (relative to control)')), xaxt='n')
axis(1,at=c(0,1,2,3,4,5,6), labels=c("Field","July-31","Aug-01", "Aug-02", "Aug-03", "Aug-04", "Aug-05"))
group=4
days=7
sym=15
errbar(0, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, col="Gray", xaxt='n')
group=7
sym=17
errbar(0, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, col="Gray", xaxt='n')
group=1
sym=19
errbar(0, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, col="Gray", xaxt='n')

group=6:5
days=1:6
sym=c(0,15)
x.pos<-c(0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1, 4.9, 5.1, 5.9, 6.1)
errbar(x.pos, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, xaxt='n')
group=9:8
sym=c(2,17)
errbar(x.pos, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, xaxt='n')
group=3:2
sym=c(1,19)
errbar(x.pos, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, xaxt='n')

#FBS
text(0, -0.15, "O*", cex=1)
#BS
text(1, -0.15, "O* A*** OxA**", cex=1)
#D0
#text(2, -0.15, "OxA**", cex=1)
#D1
text(3, -0.15, "O*", cex=1)
#D2
text(4, -0.15, "A**", cex=1)
#D3
text(5, -0.15, "A***", cex=1)
#D4
text(6, -0.15, "O*** A***", cex=1)

#overall
text(1, seq(from=2.95, to=(2.95-(5*.125)), by=-0.125), c("A*","D***","OxA***","OxD***","AxD***","OxAxD**"), adj=c(0,0))
legend("topleft",c("HV","MV","forereef","stable","variable","field baseline"), pch=c(0,2,1,0,15,15), col=c(rep("black",5),"Gray"),pt.cex=1.5, bty='n')

dev.off()

