require(c("R.utils","plyr")
library(R.utils)
library(plyr)

if (!file.exists("data")) dir.create("data")
fileURL<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
bzFile <- "./data/StormData.csv.bz2"
download.file(fileURL, destfile = bzFile)
sdFile <- gsub(".bz2","",bzFile)
bunzip2(bzFile, sdFile, remove = FALSE)  # Extract file from bz2

# read the data in and view overall statistics about it
sd <- read.csv(sdFile)
str(sd)

# convert all the EventType names to uppercase and strip off plural "s"
sdCleaned <- mutate(sd, EVTYPE = sub("S$","",toupper(EVTYPE)))

# summarize by EventType and count total Fatalities & Injuries
harmful <- ddply(sdCleaned, .(EVTYPE), summarize, count = length(EVTYPE), totFat = sum(FATALITIES),
                 totInj = sum(INJURIES), totHarm = totFat+totInj, .drop=FALSE)

# subset and look for the most harmful by total injuries/fatalities
MostHarmful <-subset(harmful, totFat>50 | totInj>50)

#
MostHarmful <-mutate(MostHarmful, harmFac = 10*totFat+totInj, perEvent = round(harmFac/count,1))

TopHarmful <- head(MostHarmful[order(-MostHarmful$harmFac),],15)
TopPerEvent <- head(MostHarmful[order(-MostHarmful$perEvent),],15)
