# Install the R packages: ggplot2, forecast
install.packages("forecast")
install.packages("ggplot2")

# Call the two libraries
require(ggplot2)
require(forecast)

# Set the working directory for the data
setwd("../data/")

dat <- read.delim(file = "tsdata.tsv", header=TRUE, stringsAsFactors=FALSE, sep="\t")

# Perform the necessary formatting of the strings

dat$Day <- as.Date(dat$Day,format="%m/%d/%y")
dat$Visits <- as.numeric(gsub(",","",dat$Visits))
dat$MobileViews <- as.numeric(gsub(",","",dat$Mobile.Views))
dat$Orders <- as.numeric(gsub(",","",dat$Orders))
dat$PageViews <- as.numeric(gsub(",","",dat$Page.Views))

# Plot the data
tname <- "Holiday"
p <- ggplot(data=dat,aes(x=Day)) +
  geom_line(aes(y=Visits, color="Visits"), size = 1, lineend = "round") +
  geom_line(aes(y=MobileViews, color="MobileViews"), size = 1, lineend = "round") +
  geom_line(aes(y=Orders, color="Orders"), size = 1, lineend = "round") +
  geom_line(aes(y=PageViews, color="PageViews"), size = 1, lineend = "round") +
  #   scale_x_date(format = "%b-%Y") + 
#   scale_color_manual("", breaks = c("Visits","MobileViews"), values = c("cyan", "magenta")) +
  theme_bw(base_size=14) +
  xlab("") + ylab("Value")
print(p)

#########################################
## Visits forecast
#########################################
y <- ts(dat$Orders,frequency=7)

## Exponential smoothing models
fit.order <- ets(y)
forecast.order <- forecast(fit.order,h=7,level=c(95))
# plot(forecast.order)
n1 = 150
datplot <- data.frame(Day=c(dat$Day[n1:198],as.Date((1:7)+dat$Day[198])),Actual=NA,Predicted=NA
                 ,Lower=NA,Upper=NA)
datplot$Actual[1:(198-n1+1)] <- y[n1:198]
datplot$Predicted <- datplot$Actual
datplot$Predicted[(198-n1+2):(198-n1+8)] <- forecast.order$mean
datplot$Lower[(198-n1+2):(198-n1+8)] <- forecast.order$lower
datplot$Upper[(198-n1+2):(198-n1+8)] <- forecast.order$upper

p <- ggplot(data=datplot, aes(x=Day)) + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.3, fill="grey80") + 
  geom_line(aes(y=Predicted, color="Predicted"), size = 1, lineend = "round") + 
  geom_line(aes(y=Actual, color="Actual"), size = 1, lineend = "round") + 
  scale_color_manual("", 
                     breaks = c("Actual", "Predicted"),
                     values = c("grey60","green")) +
  theme_bw(base_size=14) + ylab("Value") + xlab("Day")
print(p)

## Arima models
fit.order <- auto.arima(y)
forecast.order <- forecast(fit.order,h=7,level=c(95))
# plot(forecast.order)
n1 = 150
datplot <- data.frame(Day=c(dat$Day[n1:198],as.Date((1:7)+dat$Day[198])),Actual=NA,Predicted=NA
                      ,Lower=NA,Upper=NA)
datplot$Actual[1:(198-n1+1)] <- y[n1:198]
datplot$Predicted <- datplot$Actual
datplot$Predicted[(198-n1+2):(198-n1+8)] <- forecast.order$mean
datplot$Lower[(198-n1+2):(198-n1+8)] <- forecast.order$lower
datplot$Upper[(198-n1+2):(198-n1+8)] <- forecast.order$upper

p <- ggplot(data=datplot, aes(x=Day)) + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper), alpha=0.3, fill="grey80") + 
  geom_line(aes(y=Predicted, color="Predicted"), size = 1, lineend = "round") + 
  geom_line(aes(y=Actual, color="Actual"), size = 1, lineend = "round") + 
  scale_color_manual("", 
                     breaks = c("Actual", "Predicted"),
                     values = c("grey60","green")) +
  theme_bw(base_size=14) + ylab("Value") + xlab("Day")
print(p)
