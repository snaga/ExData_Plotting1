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

plot1 <- function(data)
{
  par(mfrow = c(1,1))
  
  hist(data$Global_active_power,
       main = "Global Active Power",
       xlab = "Global Active Power (kilowatts)",
       col = "red")
}

data <- loaddata("household_power_consumption.txt")
#head(data)
#tail(data)

# plot1
png(filename = "plot1.png", width = 480, height = 480)
par(ps = 12)
plot1(data)
dev.off()  
