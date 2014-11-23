# Exploratory Analysis Project #2

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Determine # of unique elements per variable
nunique <- function(x) { length(unique(x)) }
sapply(NEI, nunique)

sum(is.na(NEI)) # check if any are N/A in table
table(NEI$year) # play with looking at a table
table(NEI$year,NEI$type)

PMBaltPlot <- qplot(year, total, data=PMBaltTyYr, facets=.~type ) + xlab("Year") +
    ylab("Total Emissions (tons)") + ggtitle("Balitmore City (by Type)") + 
    geom_point(colour = "red", size = 5) + facet_wrap(~type, nrow=2,ncol=2) 

sectors <- as.character(unique(SCC$EI.Sector))
ss <- subset(SCC, EI.Sector %in% sectors[25:27])

# QUESTION 1 - Has PM2.5 decreased in the US from 1999-2008
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# use tapply to sum emissions by year and then plot with base package
PMbyYr <- tapply(NEI$Emissions, NEI$year, sum)
png(filename="plot1.png", width=480, height=480)
lab = expression("Total " ~ PM[2.5] ~ "Emissions (all sources)")
barplot(PMbyYr, names.arg=rownames(PMbyYr), main=lab, ylab="Emission (tons)")
dev.off()

# QUESTION 2 - Total emissions decreased in Baltimore City (fips=="24510")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Subset the data for Baltimore City and the sum emissions by year
NEI_BaltimoreCity <- subset(NEI, fips=="24510", select=c("Emissions","year"))
PMBaltimore <- tapply(NEI_BaltimoreCity$Emissions, NEI_BaltimoreCity$year, sum)
png(filename="plot2.png", width=480, height=480)
lab = expression("Total " ~ PM[2.5] ~ "for Baltimore City (all sources)")
barplot(PMBaltimore, names.arg=rownames(PMBaltimore), main=lab, 
        ylab="Emission (tons)")
dev.off()

# QUESTION 3 - What type of emission has seen an increase in Baltimore city?
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

library(plyr)
library(ggplot2)

# subset the data for just Baltimore City
NEI_BaltimoreCity <- subset(NEI, fips=="24510", select=c("Emissions","year","type"))

# use plyr ddply to summarize the data by type/year and then plot with ggplot
PMBaltTyYr <- ddply(NEI_BaltimoreCity, c("type", "year"), 
           summarize, N = length(Emissions), total = sum(Emissions))
PMBaltPlot <- qplot(year, total, data=PMBaltTyYr, color=type) + xlab("Year") +
    ylab("Total Emissions (tons)") + ggtitle("Balitmore City (by Type)") + 
    geom_point(size = 3) + geom_line()
ggsave("plot3.png", width=6, height=4, dpi=120)

# QUESTION 4 - How has emissions from coal related sources changed across US
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# extract unique coal sources from SCC using EI.sector and get corresponding SCCs
Sectors <- as.character(unique(SCC$EI.Sector))
coalSources <-Sectors[grep("coal", Sector, ignore.case = T)]
coalSCCs <- subset(SCC, EI.Sector %in% coalSources, select = SCC)

# subset NEI based on coal SCCs
NEI_coal<- subset(NEI, SCC %in% coalSCCs$SCC, select=c("Emissions","year","type"))

# sum up all coal emissions sources per year and plot/save the data
PMCoalSources <- tapply(NEI_coal$Emissions, NEI_coal$year, sum)
png(filename="plot4.png", width=480, height=480)
lab = expression("Total " ~ PM[2.5] ~ "Emissions (coal sources)")
barplot(PMbyYr, names.arg=rownames(PMbyYr), main=lab, ylab="Emission (tons)")
dev.off()

# QUESTION 5 - How emissions from motor vehicle sources changed in Baltimore City
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# extract unique coal sources from SCC using EI.sector and get corresponding SCCs
Sectors <- as.character(unique(SCC$EI.Sector))
motorVehicleSources <- Sectors[21:24]
mvSCCs <- subset(SCC, EI.Sector %in% motorVehicleSources, select = SCC)

# subset NEI based on motor vehicle SCCs and Baltimore City
NEI_BC_MotorVehicle <- subset(NEI, SCC %in% mvSCCs$SCC & fips=="24510")

# sum emissions by year and then create bar plot
PMBalt_MV <- tapply(NEI_BC_MotorVehicle$Emissions, NEI_BC_MotorVehicle$year, sum)
png(filename="plot5.png", width=480, height=480)
lab = expression("Total " ~ PM[2.5] ~ "for Baltimore City (motor vehicle sources)")
barplot(PMBalt_MV, names.arg=rownames(PMBalt_MV), main=lab, ylab="Emission (tons)")
dev.off()

# QUESTION 6 - Compare Baltimore City with LA County for motor vehicle emissions
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# extract unique coal sources from SCC using EI.sector and get corresponding SCCs
Sectors <- as.character(unique(SCC$EI.Sector))
motorVehicleSources <- Sectors[21:24] # select 4 motor vehicle factors
mvSCCs <- subset(SCC, EI.Sector %in% motorVehicleSources, select = SCC)

# subset NEI based on motor vehicle SCCs and LA County / Baltimore City
NEI_MotorVehicles<- subset(NEI, SCC %in% mvSCCs$SCC & fips %in% c("24510", "06037"))

# load plyr for ddply and gridExtra for doing a 2 grid layout
library(plyr)
require(gridExtra)

# use ddply to summarize data by fips and year and create variables N, total
PMByCityYear <- ddply(NEI_MotorVehicles, c("fips", "year"), 
                    summarize, N = length(Emissions), total = sum(Emissions))

# use mutate to add a baseline emission value, calculated % change, and city
PMByCityYear <- mutate(PMByCityYear, 
            baseline = ifelse(fips==24510, 346.82, 3931.12),
            change = ((total-baseline)/baseline + 1)*100, 
            city = ifelse(fips==24510, "Baltimore City", "LA County"))

# plot relative (plot 1) and absolute (plot 2) changes for boh locations
plot1 <- qplot(year, change, data = PMByCityYear, color = city) + 
    ylab("Relative (%) Change in Total Emissions") + ggtitle("Motor Vehicle Emissions") + 
    geom_line() + geom_point(size = 3)
plot2 <- qplot(year, total, data = PMByCityYear, color = city)+ xlab("Year") +
    ylab("Total Emissions (tons)") + geom_line() + geom_point(size = 3)

# plot to local device and save as png
png(filename="plot6.png", width=480, height=640)
grid.arrange(plot1, plot2, nrow=2)
dev.off()
