## Quiz #4
# Question #1
if (!file.exists("data")) dir.create("data")
fileURL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile = "./data/2006microdataSurvey.csv")

mds <- read.csv("./data/2006microdataSurvey.csv")
strsplit(names(mds), "wgtp")[123]

# Question #2
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileURL, destfile = "./data/getdata_data_FGDP.csv")

gdp<-read.csv("./data/getdata_data_FGDP.csv")
gdp$X.3 <- as.numeric.factor(gdp$X.3)
countries <- gdp[-which(gdp$Gross.domestic.product.2012==''),]
mean(countries$X.3,na.rm = T)

# Question #3
gdp[grep("^United",gdp$X.2),]

# Question #4
fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileURL2, destfile = "./data/EDU.csv")
EDU <- read.csv("./data/EDU.csv")
colnames(gdp)[c(1,2,4,5)] = c("shortCode","Rank","Country","GDP")
GDPEDU <- merge(gdp, EDU, by.x="shortCode", by.y="CountryCode")
snJune <- grep('end: [jJ]une', GDPEDU$Special.Notes)
GDPEDU[snJune,c('Country','Special.Notes')]
length(snJune)

# Question 5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
lngDate <-format(sampleTimes,"%a %b %d, %Y")
vals2012 <- grep(", 2012", lngDate)
length(vals2012)
mons2012 <- grep("Mon", lngDate[vals2012])
length(mons2012)
