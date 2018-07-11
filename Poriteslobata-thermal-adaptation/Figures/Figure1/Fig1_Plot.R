setwd("~/dansstuff/Projeks/Grad_and_Postdocs/Hawaii Projects/Samoa Project/San Francisco/TankTemps")

library(Hmisc)

Temps<-read.delim("AcclimationTempData_noRamp.txt", header=T)
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

Tank1DayMaxSD<-sd(Tank1DayMax)
Tank2DayMaxSD<-sd(Tank2DayMax)
Tank1DayMinSD<-sd(Tank1DayMins)
Tank2DayMinSD<-sd(Tank2DayMins)

Tank1Maxerror<-qnorm(0.975)*Tank1DayMaxSD/sqrt(length(Tank1DayMax))
Tank2Maxerror<-qnorm(0.975)*Tank2DayMaxSD/sqrt(length(Tank2DayMax))
Tank1Minerror<-qnorm(0.975)*Tank1DayMinSD/sqrt(length(Tank1DayMins))
Tank2Minerror<-qnorm(0.975)*Tank2DayMinSD/sqrt(length(Tank2DayMins))

sum((Tank1DayMax-Tank1DayMins)>3)
sum((Tank2DayMax-Tank2DayMins)>3)

mean((Tank2DayMax-Tank2DayMins)/(Tank1DayMax-Tank1DayMins))


pdf("Fig1_ComboMeanMinMax_Boxplot_unedited.pdf")
par(mfrow=c(1,2))

errbar(c(0.25,0.75),c(mean(Tank1DayMeans),mean(Tank2DayMeans)), c(mean(Tank1DayMeans)+mean(Tank1Dayerror),mean(Tank2DayMeans)+mean(Tank2Dayerror)), c(mean(Tank1DayMeans)-mean(Tank1Dayerror),mean(Tank2DayMeans)-mean(Tank2Dayerror)),pch=0, ylab='Daily Temperature °C',ylim=c(27,32),xlim=c(0:1),type='p',cex=4, xaxt='n', xlab='Acclimation Tank')
axis(side=1,at=c(0.25,0.75),labels=c("Stable", "Variable"))

errbar(c(0.25,0.75), c(mean(Tank1DayMax),mean(Tank2DayMax)), c(mean(Tank1DayMax)+Tank1Maxerror,mean(Tank2DayMax)+Tank2Maxerror), c(mean(Tank1DayMax)-Tank1Maxerror,mean(Tank2DayMax)-Tank2Maxerror), pch=20, type='p', cex=1, add=T)

errbar(c(0.25,0.75), c(mean(Tank1DayMins),mean(Tank2DayMins)), c(mean(Tank1DayMins)+Tank1Minerror,mean(Tank2DayMins)+Tank2Minerror), c(mean(Tank1DayMins)-Tank1Minerror,mean(Tank2DayMins)-Tank2Minerror), pch=20, type='p', cex=1, add=T)

boxplot(data.frame("Stable"=Tank1DayMax-Tank1DayMins, "Variable"=Tank2DayMax-Tank2DayMins), xlab='Acclimation Tank', ylim=c(0,5.25),ylab='Daily Temperature Range °C')


dev.off()
