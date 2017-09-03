# Exploratory Data Analysis - Final project - Q6

# first way. Note: check why the ggplot does not produce a plot
getwd()
# Load ggplot2 library
library(ggplot2)

NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

NEI$year <- factor(NEI$year, levels=c('1999', '2002', '2005', '2008'))

# Baltimore City, Maryland
# Los Angeles County, California
MD.onroad <- subset(NEI, fips == '24510' & type == 'ON-ROAD')
CA.onroad <- subset(NEI, fips == '06037' & type == 'ON-ROAD')

# use aggregate
MD.DF <- aggregate(MD.onroad[, 'Emissions'], by=list(MD.onroad$year), sum)
colnames(MD.DF) <- c('year', 'Emissions')
MD.DF$City <- paste(rep('MD', 4))

CA.DF <- aggregate(CA.onroad[, 'Emissions'], by=list(CA.onroad$year), sum)
colnames(CA.DF) <- c('year', 'Emissions')

CA.DF$City <- paste(rep('CA', 4))

DF <- as.data.frame(rbind(MD.DF, CA.DF))

# Question 6 - Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == 06037). Which city has seen greater changes over time 
# in motor vehicle emissions?

# Generate the graph 
png('/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot6.png')

ggplot(data=DF, aes(x=year, y=Emissions)) + geom_bar(aes(fill=year)) + guides(fill=F) + 
  ggtitle('Total Emissions of Motor Vehicle Sources\nLos Angeles County, California vs. Baltimore City, Maryland') + 
  ylab(expression('PM'[2.5])) + xlab('Year') + theme(legend.position='none') + facet_grid(. ~ City) + 
  geom_text(aes(label=round(Emissions,0), size=1, hjust=0.5, vjust=-1))

dev.off()



# second way

# Exploratory Data Analysis - Final project - Q6

## Load the Libraries needed: 
library(plyr)
library(ggplot2)
library(grid)

## Step 1: read in the data
NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

## Step 2: check the levels for types of vehicles defined
mv.sourced <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, value = TRUE))

mv.sourcec <- SCC[SCC$EI.Sector %in% mv.sourced, ]["SCC"]


## Step 3A: subset our data Baltimore City
emMV.ba <- NEI[NEI$SCC %in% mv.sourcec$SCC & NEI$fips == "24510", ]
## Step 3B: subset our data Los Angeles County
emMV.LA <- NEI[NEI$SCC %in% mv.sourcec$SCC & NEI$fips == "06037", ]

## Step 3C: bind the data created in steps 3A and 3B
emMV.comb <- rbind(emMV.ba, emMV.LA)

## Step 4: Find the emmissions due to motor vehicles in 
## Baltimore (city) and Los Angeles County
tmveYR.county <- aggregate (Emissions ~ fips * year, data =emMV.comb, FUN = sum ) 
tmveYR.county$county <- ifelse(tmveYR.county$fips == "06037", "Los Angeles", "Baltimore")

## Step 5: plotting to png
png('/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot6.png')
qplot(year, Emissions, data=tmveYR.county, geom="line", color=county) + ggtitle(expression("Motor Vehicle Emission Levels" ~ PM[2.5] ~ "  from 1999 to 2008 in Los Angeles County, CA and Baltimore, MD")) + xlab("Year") + ylab(expression("Levels of" ~ PM[2.5] ~ " Emissions"))
dev.off()
