dat <- read.csv("VisitorsforAnalysisinR.txt", header=FALSE)
names(dat) <- c("Country", "Visits", "TrackingID", "PageViews", "Orders", "VisitDuration", "Revenue")
str(dat)

# Look convert variables to numeric
dat$NumPageViews <- as.numeric(gsub(",", "", as.character(dat$PageViews)))
dat[is.na(dat$NumPageViews),]

dat$VisitDurationSecs <- as.numeric(substr(as.character(dat$VisitDuration), 1,2))*60 + as.numeric(substr(as.character(dat$VisitDuration), 4,5))

dat$OrderFlag <- (dat$Orders > 0)*1

dat$RevenueNum <- as.numeric(gsub("[$,]", "", as.character(dat$Revenue)))
  
  

# Reduce the cardinality of the Country variable
sort(table(dat$Country))

process_categorical <- function(varname, dat)
{
  a <- dat[,varname]
  if(varname==4 || varname==8)
  {
    a <- as.numeric(a)
    a <- discretize(a, categories=nclass.Sturges(a))
  }
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

# Build linear model for revenue

fit <- lm(RevenueNum ~ CountryReduced + Visits + NumPageViews + VisitDurationSecs, 
          data = subset(dat, RevenueNum>0))
summary(fit)

fit$fitted.values


# Score the non-buyers in the data
nonbuyers <- subset(dat, RevenueNum==0)
nonbuyers$PredictedRevenue <- predict.lm(fit, newdata=nonbuyers)

ord <- order(nonbuyers$PredictedRevenue, decreasing = TRUE)
nonbuyers <- nonbuyers[ord,]

# Create segments based on high opportunity non-buyers

plot(density(nonbuyers$PredictedRevenue), xlim=c(200,450))
quantile(nonbuyers$PredictedRevenue, 0.7)

# Dataset for export
exportdata <- as.data.frame(nonbuyers[nonbuyers$PredictedRevenue > 314,"TrackingID"])
exportdata$Binary <- 1
exportdata$TimeUTC <- round(as.numeric(Sys.time()))
names(exportdata)[1] <- "AAMID"

# Write the export File
write.table(exportdata,#[1:100,], 
            file="export.txt", 
            col.names=FALSE, 
            row.names=FALSE, 
            quote=FALSE,
            sep=", ")

