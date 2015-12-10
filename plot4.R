dtf <- function(r) {
  rd <- r["Date"]
  rt <- r["Time"]
  if (is.na(rd) || is.na(rt)) {
    return(NA)
  } else {
    return(paste(rd, rt))
  }
}

dat <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", colClasses = c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), stringsAsFactors = FALSE)
dat$DateParsed <- as.Date(dat$Date, format = "%d/%m/%Y")
sd <- as.Date("2007-02-01")
ed <- as.Date("2007-02-02")
df <- dat[dat$DateParsed >= sd & dat$DateParsed <= ed, ]
dts <- apply(df, 1, dtf)
df$DateTimeParsed <- strptime(dts, format = "%d/%m/%Y %H:%M:%S")

png(filename = "plot4.png")

par(mfrow = c(2, 2))

plot(df$DateTimeParsed, df$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")

plot(df$DateTimeParsed, df$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")

plot(df$DateTimeParsed, df$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
lines(df$DateTimeParsed, df$Sub_metering_2, type="l", col = "red")
lines(df$DateTimeParsed, df$Sub_metering_3, type="l", col = "blue")
legend("topright", col = c("black", "red", "blue"),
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1,
       bty = "n")

plot(df$DateTimeParsed, df$Global_reactive_power, type = "S", xlab = "datetime", ylab = "Global_reactive_power")

dev.off()