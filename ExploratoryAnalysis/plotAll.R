# Read in Data File
df = "household_power_consumption.txt"
hpc<-read.delim(df, header=T, sep=";", na.strings="?")

# View summary stats for dataframe
summary(hpc)
str(hpc)

# Convert date/times into datetime format and select subset of data to work with
hpc$datetime = strptime(paste(hpc$Date,hpc$Time), format="%d/%m/%Y %H:%M:%S")
sd = strptime("01/02/2007 00:00:00", format="%d/%m/%Y %H:%M:%S")
ed = strptime("03/02/2007 00:00:00", format="%d/%m/%Y %H:%M:%S")
twoDays<-subset(hpc, datetime>=sd & datetime<ed, select=3:10)

# View updated stats
summary(twoDays)
str(twoDays)

xname="Global Active Power (kilowatts)"
yname="Frequency"
title="Global Active Power"
# Plot #1 - Historgram of Power Usage by kilowatts
png(filename="plot1.png", width=480, height=480)
hist(twoDays$Global_active_power, 
     xlab="Global Active Power (kilowatts)", 
     ylab="Frequency", 
     main="Global Active Power", 
     col="red")
dev.off()

# Plot #2 - Global Active Power over time
png(filename="plot2.png", width=480, height=480)
plot(twoDays$datetime, twoDays$Global_active_power,type="l",xlab="",
     ylab="Global Active Power (kilowatts")
dev.off()

# plot #3 - Sub metering as a function of time
png(filename="plot3.png", width=480, height=480)
plot(twoDays$datetime, twoDays$Sub_metering_1,type="l",xlab="",
     ylab="Energy sub metering")
par(new="T")
lines(twoDays$datetime, twoDays$Sub_metering_2, col="red")
lines(twoDays$datetime, twoDays$Sub_metering_3, col="blue")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       lty=rep(1,3), col=c("black","red","blue"))
dev.off()

# plot #4 - 4 subplots
png(filename="plot4.png", width=480, height=480)
par(mfrow=c(2,2))
plot(twoDays$datetime, twoDays$Global_active_power,type="l",xlab="",
     ylab="Global Active Power")
plot(twoDays$datetime, twoDays$Voltage,type="l",xlab="",ylab="Voltage")
plot(twoDays$datetime, twoDays$Sub_metering_1,type="l",xlab="",
     ylab="Energy sub metering")
par(new="T")
lines(twoDays$datetime, twoDays$Sub_metering_2, col="red")
lines(twoDays$datetime, twoDays$Sub_metering_3, col="blue")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
       lty=rep(1,3), col=c("black","red","blue"))
yname="global_reactive_power"
plot(twoDays$datetime, twoDays$Global_reactive_power,type="l",
     xlab=colnames(twoDays)[8],ylab=colnames(twoDays)[2])
dev.off()

