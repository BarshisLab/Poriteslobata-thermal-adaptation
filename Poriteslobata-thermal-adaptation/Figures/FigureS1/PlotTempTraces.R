library(Hmisc)

Temps<-read.delim("AcclimationTempData_wRamp.txt", header=T)
Temps$Date.Time<-strptime(Temps$Date.Time, format="%m/%d/%y %H:%M")
Tank1DayMeans<-tapply(Temps$Tank.1, Temps$NumDate, mean)
Tank2DayMeans<-tapply(Temps$Tank.2, Temps$NumDate, mean)

Tank1DayMins<-tapply(Temps$Tank.1, Temps$NumDate, min)
Tank2DayMins<-tapply(Temps$Tank.2, Temps$NumDate, min)

Tank1DayMax<-tapply(Temps$Tank.1, Temps$NumDate, max)
Tank2DayMax<-tapply(Temps$Tank.2, Temps$NumDate, max)

Tank1DaySD<-tapply(Temps$Tank.1, Temps$NumDate, sd)
Tank2DaySD<-tapply(Temps$Tank.2, Temps$NumDate, sd)

Tank1Dayerror<-qnorm(0.975)*Tank1DaySD/sqrt(tapply(Temps$Tank.1, Temps$NumDate, length))
Tank2Dayerror<-qnorm(0.975)*Tank2DaySD/sqrt(tapply(Temps$Tank.1, Temps$NumDate, length))

Tank1error<-qnorm(0.975)*mean(Tank1DaySD)/sqrt(length(Tank1DaySD))
Tank2error<-qnorm(0.975)*mean(Tank2DaySD)/sqrt(length(Tank2DaySD))

Data4Plot<-data.frame(cbind(Tank1DayMeans, Tank2DayMeans, Tank1DayMax, Tank2DayMax, Tank1DayMins, Tank2DayMins, Tank1DaySD, Tank2DaySD, Tank1Dayerror, Tank2Dayerror))
Data4Plot<-cbind("Days"=strptime(names(Tank1DayMeans), format="%m/%d/%y"), Data4Plot)


pdf("FigS1_AcclimationTempPlot_wRamp.pdf")
plot(Temps$Date.Time, Temps$Tank.1, type="l", lwd=2, ylab="Aquarium Temperature °C", xlab="Date and Time (2007)", main="Acclimation Tank Temperatures for 36days of Acclimation
and 5 Day Thermal Challenge", col="Blue", ylim=c(25.5,40), xaxt='n')
points(Temps$Date.Time, Temps$Tank.2, type="l", lwd=2, col="Red")
ticks<-seq(from=min(Data4Plot$Days), by='1 day', length=41)
lbl<-strftime(ticks, format="%b-%d")
axis(side=1, at=ticks, labels=F)
text(ticks, par("usr")[3] - 0.5, srt=60, adj=1 , labels=lbl, xpd = T, cex=0.75)
legend("bottomleft",c("Stable","Variable"),col=c("Blue","Red"), pch="-", pt.cex=3)

dev.off()
