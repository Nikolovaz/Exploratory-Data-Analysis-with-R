# Exploratory Data Analysis - Final project - Q5

# first way. Note: check why the ggplot does not produce a plot
getwd()
# Load ggplot2 library
library(ggplot2)

# Loading the provided datasets
NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

NEI$year <- factor(NEI$year, levels=c('1999', '2002', '2005', '2008'))

# Baltimore City, Maryland == fips
MD.onroad <- subset(NEI, fips == 24510 & type == 'ON-ROAD')

# use aggregate
MD.df <- aggregate(MD.onroad[, 'Emissions'], by=list(MD.onroad$year), sum)
colnames(MD.df) <- c('year', 'Emissions')

# Question 5 - How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 

# Generate the graph
png('/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot5.png')


ggplot(data=MD.df, aes(x=year, y=Emissions)) + geom_bar(aes(fill=year)) + guides(fill=F) + 
  ggtitle('Total Emissions of Motor Vehicle Sources in Baltimore City, Maryland') + 
  ylab(expression('PM'[2.5])) + xlab('Year') + theme(legend.position='none') + 
  geom_text(aes(label=round(Emissions,0), size=1, hjust=0.5, vjust=2))

dev.off()

# second way


# Exploratory Data Analysis - Final project - Q5

## Libraries needed:
library(plyr)
library(ggplot2)

## Step 1: Loading the provided datasets
NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

## Step 2A: subset our data 
## Assumptions: motor vehicles = On and
###check the levels for types of vehicles defined
mv.sourced <- unique(grep("Vehicles", SCC$EI.Sector, ignore.case = TRUE, value = TRUE))

mv.sourcec <- SCC[SCC$EI.Sector %in% mv.sourced, ]["SCC"]

##Step 2B: subset the emissions from motor vehicles from
##NEI for Baltimore, MD.
emMV.ba <- NEI[NEI$SCC %in% mv.sourcec$SCC & NEI$fips == "24510",]

## Step 3: find the emissions due to motor vehicles in Baltimore for every year
balmv.pm25yr <- ddply(emMV.ba, .(year), function(x) sum(x$Emissions))
colnames(balmv.pm25yr)[2] <- "Emissions"

## Step 4: Generate the graph
png('/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot5.png')
qplot(year, Emissions, data=balmv.pm25yr, geom="line") + ggtitle(expression("Baltimore City" ~ PM[2.5] ~ "Motor Vehicle Emissions by Year")) + xlab("Year") + ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))
dev.off()
