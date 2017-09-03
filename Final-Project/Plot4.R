# Exploratory Data Analysis - Final project - Q4

getwd()
# Load ggplot2 library
require(ggplot2)

# Loading the provided datasets
NEI <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/exdata%2Fdata%2FNEI_data/Source_Classification_Code.rds")

# Coal combustion related sources
SCC.coal = SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]

# Merge two data sets
merge <- merge(x=NEI, y=SCC.coal, by='SCC')
merge.sum <- aggregate(merge[, 'Emissions'], by=list(merge$year), sum)
colnames(merge.sum) <- c('Year', 'Emissions')

# Question 4 - Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999-2008?

# Generate the graph
png(filename='/Users/petya/Desktop/Data_science/JH_university_course/ExploratoryDataA/week4/plot4.png')

ggplot(data=merge.sum, aes(x=Year, y=Emissions/1000)) + 
  geom_line(aes(group=1, col=Emissions)) + geom_point(aes(size=2, col=Emissions)) + 
  ggtitle(expression('Total Emissions of PM'[2.5])) + 
  ylab(expression(paste('PM', ''[2.5], ' in kilotons'))) + 
  geom_text(aes(label=round(Emissions/1000,digits=2), size=2, hjust=1.5, vjust=1.5)) + 
  theme(legend.position='none') + scale_colour_gradient(low='black', high='red')

dev.off()
