## Quiz #3
# Question #1
if (!file.exists("data")) dir.create("data")
fileURL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile = "./data/2006microdataSurvey.csv")
list.files("data")
ms<-read.csv("./data/2006microdataSurvey.csv")
str(ms)
summary(ms)
table(ms$ACR) # ACR # of acres, 3 - 10+
table(ms$AGS) # AGS agricultural sales, 6 - >$10k

agriculturalLogical <- ifelse(ms$ACR==3 & ms$AGS==6, TRUE, FALSE)
which(agriculturalLogical)

# Question #2
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
install.packages("jpeg")
library(jpeg)
lsf.str("package:jpeg")
download.file(fileURL, destfile = "./data/instructor.jpg")
img <- readJPEG("./data/jeff.jpg", native=TRUE)
quantile(img, c(0, .3, .8, 1))

# Question #3
fileURL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileURL2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(fileURL, destfile="./data/GDP.csv")
download.file(fileURL2, destfile = "./data/EDU.csv")
list.files("data")

as.numeric.factor <- function(x) {as.numeric(gsub(",","",levels(x)))[x]}

GDP <- read.csv("./data/GDP.csv", skip=4, col.names = paste0("c",1:10), nrows=190)
colnames(GDP)[c(1,2,4,5)] = c("shortCode","Rank","Country","GDP")
head(GDP)
GDP$GDP <- as.numeric.factor(GDP$GDP)
EDU <- read.csv("./data/EDU.csv")

summary(EDU)
str(GDP)
head(GDP)

GDPEDU <- merge(GDP, EDU, by.x="shortCode", by.y="CountryCode")
GDPEDU <- GDPEDU[order(GDPEDU[,"GDP"], decreasing=FALSE),]
dim(GDPEDU)
GDPEDU[13,]

# Question 4
tapply(GDPEDU,Income.Group, mean)

# Question 5
install.packages("Hmisc")
library(Hmisc)
GDPEDU$rankFactor = cut2(GDPEDU$Rank,g=5)
GDPEDU <- GDPEDU[order(GDPEDU[,"GDP"], decreasing=T),]
table(GDPEDU$rankFactor)
table(GDPEDU$rankFactor, GDPEDU$Income.Group)
