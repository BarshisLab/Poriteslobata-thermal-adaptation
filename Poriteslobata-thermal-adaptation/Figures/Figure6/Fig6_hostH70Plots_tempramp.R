library(Hmisc)

host<-read.delim("San_Francisco_hostTempramp.txt", header=T)
head(host)

hsp70mean=tapply(host$Hsp70,list(host$Group,host$Day), mean)

hsp70mean

hsp70SD=tapply(host$Hsp70,list(host$Group,host$Day), sd)

hsp70N=tapply(host$Hsp70,list(host$Group,host$Day), length)
hsp70N

hsp70SE=hsp70SD/sqrt(hsp70N)
hsp70SE


pdf(file="Fig6_AllDaysHostHsp70_SD.pdf", 10, 7)
par(mar=c(7,6,2,5))
sym=15
errbar(0:6, hsp70mean[2,], pch=sym, type="n", ylim=c(0,6), xlim=c(-0.25,6.5), xlab='', ylab="Host Hsp70 Levels (relative to control)", xaxt='n')
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

group=5:6
days=1:6
sym=c(0,15)
x.pos<-c(0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1, 4.9, 5.1, 5.9, 6.1)
errbar(x.pos, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, xaxt='n')
group=8:9
sym=c(2,17)
errbar(x.pos, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, xaxt='n')
group=2:3
sym=c(1,19)
errbar(x.pos, hsp70mean[group,days], hsp70mean[group,days]+hsp70SD[group,days], hsp70mean[group,days]-hsp70SD[group,days],  add=TRUE, pch=sym, type="b", cex=2, xaxt='n')

#BS
text(1, 0.10, "A*", cex=1)
#D1
text(3, 0.10, "O*", cex=1)
#D2
text(5, 0.10, "O*", cex=1)
text(1, seq(from=5.75, to=(5.75-(2*.25)), by=-0.25), c("O*","D***","OxD***"), adj=c(0,0))
legend("topleft",c("HV","MV","forereef","stable","variable","field baseline"), pch=c(0,2,1,0,15,15), col=c(rep("black",5),"Gray"),pt.cex=1.5, bty='n')

dev.off()