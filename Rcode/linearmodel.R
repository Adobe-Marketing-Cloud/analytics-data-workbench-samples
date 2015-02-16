######################################################
##              Build Linear Model in R             ##
######################################################


# Set the working directory for the data
setwd("../data/")

# Read in the Data
dat <- read.csv("VisitorsforAnalysisinR.txt", header=FALSE)
# Give names to the variables
names(dat) <- c("Country", "Visits", "TrackingID", "PageViews", "Orders", "VisitDuration", "Revenue")
# Look at the structure of the data
str(dat)

## Perform the necessary cleaning up of the character strings
# Convert variables to numeric
dat$NumPageViews <- as.numeric(gsub(",", "", as.character(dat$PageViews)))
dat$VisitDurationSecs <- as.numeric(substr(as.character(dat$VisitDuration), 1,2))*60 + as.numeric(substr(as.character(dat$VisitDuration), 4,5))
dat$OrderFlag <- (dat$Orders > 0)*1
dat$RevenueNum <- as.numeric(gsub("[$,]", "", as.character(dat$Revenue)))
  
  
## Reduce the cardinality of the Country variable
sort(table(dat$Country))

# Write a function, that checks if the variable has more that 8 classes, and clubs the
# less frequent classes into the "other" group.

process_categorical <- function(varname, dat)
{
  a <- dat[,varname]
  if(class(a)=="factor" & length(table(a))>8)
  {
    sum(table(a))
    table(a)
    fulllist <- levels(a)
    maxkeep <- min(9, sum(table(a)/sum(table(a)) >0.001))
    keep <- names(sort(table(a), decreasin=TRUE))[1:maxkeep]
    levels(a)[levels(a) %in% setdiff(fulllist, keep)] <- "other"
  }
  return(a)
}

dat$CountryReduced <- process_categorical(1, dat)

# Build linear model for revenue on this processed data
fit <- lm(RevenueNum ~ CountryReduced + Visits + NumPageViews + VisitDurationSecs, 
          data = subset(dat, RevenueNum>0))
# Look at the summary of this fit
summary(fit)

# Create a database on the non-buyers in the dataset
nonbuyers <- subset(dat, RevenueNum==0)
# Score the non-buyers with their predicted Revenue if they had purchased
nonbuyers$PredictedRevenue <- predict.lm(fit, newdata=nonbuyers)

#ord <- order(nonbuyers$PredictedRevenue, decreasing = TRUE)
#nonbuyers <- nonbuyers[ord,]

# Create segments based on high opportunity non-buyers
ggplot(nonbuyers, aes(x=PredictedRevenue)) + geom_density() + theme_bw(14) + xlim(250, 400)

# Compute the 75 percentile of the predicted revenue
quantile(nonbuyers$PredictedRevenue, 0.75)

# Dataset for export
exportdata <- as.data.frame(nonbuyers[nonbuyers$PredictedRevenue > 317,"TrackingID"])
exportdata$Binary <- 1
exportdata$TimeUTC <- round(as.numeric(Sys.time()))
names(exportdata)[1] <- "AAMID"

# Write the export File
write.table(exportdata, 
            file="export.txt", 
            col.names=FALSE, 
            row.names=FALSE, 
            quote=FALSE,
            sep=", ")

