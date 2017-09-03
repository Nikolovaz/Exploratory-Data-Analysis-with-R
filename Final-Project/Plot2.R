# Exploratory Data Analysis - Final project - Q2

getwd()
# Loading the provided datasets
NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

# Sampling
NEI_sampling <- NEI[sample(nrow(NEI), size=5000, replace=F), ]

# Subset data and append two years in one data frame
MD <- subset(NEI, fips=='24510')

# Question 2 - Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# Generate the graph
png(filename='/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot2.png')

barplot(tapply(X=MD$Emissions, INDEX=MD$year, FUN=sum), 
        main='Total Emission in Baltimore City, MD', 
        xlab='Year', ylab=expression('PM'[2.5]))

dev.off()
