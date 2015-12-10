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

png(filename = "plot1.png")
hist(df$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()