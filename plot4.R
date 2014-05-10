loaddata <- function(file)
{
  data <- read.csv(file, sep = ";", na.strings = "?")
  
  #  names(data)
  #  head(data)
  
  #  d <- head(data$Date)
  #  d
  #  strptime(data$Date, "%d/%m/%Y")
  #  as.Date(strptime(data$Date, "%d/%m/%Y"))
  #  t <- head(data$Time)
  #  t
  #  strptime(t, "%H:%M:%S")
  #  nrow(data)
  
  data2 <- subset(data, as.Date(strptime(data$Date, "%d/%m/%Y")) >= "2007-02-01")
  data3 <- subset(data2, as.Date(strptime(data2$Date, "%d/%m/%Y")) <= "2007-02-02")
  #  nrow(data3)
  
  data3
}

xaxis_wday <- function(data)
{
  Sys.setlocale(category = "LC_TIME", locale = "C")
  Sys.setlocale(category = "LC_ALL", locale = "C")
  
  # put "wday" on x-axis at "00:00:00".
  d <- data[(data$Time == "00:00:00"),]
  rownum <- as.integer(rownames(d)) - min(as.integer(rownames(data))) + 1
  l <- format(as.Date(strptime(d$Date, "%d/%m/%Y")), "%a")
  #rownum
  #l
  axis(1, at = rownum, labels=l)
  
  # right end
  d <- data[nrow(data), ]
  l <- format(as.Date(strptime(d$Date, "%d/%m/%Y"))+1, "%a")
  rownum <- nrow(data)
  axis(1, at = rownum, labels=l)
}

plot4 <- function(data)
{
  par(mfrow = c(2,2))
  
  # chart1: Global Active Power
  plot(data$Global_active_power,
       type = "l",
       col = "black",
       xaxt = "n",
       xlab = "",
       ylab = "Global Active Power")
  xaxis_wday(data)
  
  # chart2: VoltageGlobal Active Power
  plot(data$Voltage,
       type = "l",
       col = "black",
       xaxt = "n",
       xlab = "datetime",
       ylab = "Voltage")
  xaxis_wday(data)
  
  # chart3: Energy sub metering
  ymax <- max(data$Sub_metering_1, data$Sub_metering_2, data$Sub_metering_3)
  
  plot(data$Sub_metering_1,
       type = "l",
       col = "black",
       xaxt = "n",
       xlab = "",
       ylab = "Energy sub metering",
       ylim = c(0, ymax))
  par(new=T)
  plot(data$Sub_metering_2,
       type = "l",
       col = "red",
       xaxt = "n",
       xlab = "",
       ylab = "Energy sub metering",
       ylim = c(0, ymax))
  par(new=T)
  plot(data$Sub_metering_3,
       type = "l",
       col = "blue",
       xaxt = "n",
       xlab = "",
       ylab = "Energy sub metering",
       ylim = c(0, ymax))
  
  legend("topright", lty=1, col = c("black", "red", "blue"),
         bty = "n",
         legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  xaxis_wday(data)
  
  # chart4: Global_reactive_power
  plot(data$Global_reactive_power,
       type = "l",
       col = "black",
       xaxt = "n",
       xlab = "datetime",
       ylab = "Global_reactive_power")
  xaxis_wday(data)
}

data <- loaddata("household_power_consumption.txt")
#head(data)
#tail(data)

# plot4
png(filename = "plot4.png", width = 480, height = 480)
par(ps = 12)
plot4(data)
dev.off()  
