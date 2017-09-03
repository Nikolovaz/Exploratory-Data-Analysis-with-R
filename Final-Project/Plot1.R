# Exploratory Data Analysis - Final Project - Q1

getwd()
# Loading the provided datasets 
NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

# Sampling
NEI_sampling <- NEI[sample(nrow(NEI), size=2000, replace=F), ]

# Aggregate
Emissions <- aggregate(NEI[, 'Emissions'], by=list(NEI$year), FUN=sum)
Emissions$PM <- round(Emissions[,2]/1000,2)

# Question 1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

# Generate the graph 
png(filename='/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot1.png')

barplot(Emissions$PM, names.arg=Emissions$Group.1, 
        main=expression('Total Emission of PM'[2.5]),
        xlab='Year', ylab=expression(paste('PM', ''[2.5], ' in Kilotons')))

dev.off()
