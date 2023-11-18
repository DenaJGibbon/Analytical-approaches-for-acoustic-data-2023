# First load the required libraries -------------------------------------------------------------------------
library(stringr)
library(ggpubr)
library(dplyr)

# Load the Ithaca weather data -------------------------------------------------------------------------
# The weather data comes from the Global Historical Climatology Network (NOAA National Climatic Data Center. http://doi.org/10.7289/V5D21VHZ )
IthacaWeatherData <- read.csv("data/BiodiversityAnalysis/Ithaca_weather_data.csv")

# Get unique values in the 'NAME' column
unique(IthacaWeatherData$NAME)

# Subset the data for Ithaca Cornell University
IthacaSubset <- subset(IthacaWeatherData, NAME == "ITHACA CORNELL UNIVERSITY, NY US")

# Display the first few rows of the subset
head(IthacaSubset)

# Split the 'DATE' column into Year, Month, Day, and Month_Day
# This line uses the 'str_split_fixed' function to split the 'DATE' column of the 'IthacaSubset' data frame into Year, Month, and Day components, using '-' as the delimiter. The result is stored in the 'DateSplit' variable as a matrix.
DateSplit <- str_split_fixed(IthacaSubset$DATE, pattern = '-', n = 3) 

# Assign the first column of the 'DateSplit' matrix (which contains the year) to a new column 'Year' in the 'IthacaSubset' data frame.
IthacaSubset$Year <- DateSplit[, 1] 

# Assign the third column of the 'DateSplit' matrix (which contains the day) to a new column 'Day' in the 'IthacaSubset' data frame.
IthacaSubset$Day <- DateSplit[, 3] 

# Assign the second column of the 'DateSplit' matrix (which contains the month) to a new column 'Month' in the 'IthacaSubset' data frame.
IthacaSubset$Month <- DateSplit[, 2] 

# Create a new column 'Month_Day' in the 'IthacaSubset' data frame by pasting together the 'Month' and 'Day' columns with an underscore '_' separator.
IthacaSubset$Month_Day <- paste(IthacaSubset$Month, IthacaSubset$Day, sep = '_') 

# Create boxplots for TMIN and PRCP by Month
ggboxplot(data = IthacaSubset, x = 'Month', y = 'TMIN') 

ggboxplot(data = IthacaSubset, x = 'Month', y = 'PRCP') 

# Create a histogram of the weather variables
gghistogram(data = IthacaSubset, x = 'PRCP') 

gghistogram(data = IthacaSubset, x = 'TMIN') 

gghistogram(data = IthacaSubset, x = 'TMAX') 


# Read in the Sapsucker Woods annotation data -----------------------------

# Read the SSW annotations from a CSV file
SSWAnnotations <- read.csv("data/BiodiversityAnalysis/SSWannotations.csv")

# Extract the Month, Day, and Month_Day from the 'DATE' column and create new column
SSWAnnotations$DATE <- str_split_fixed(SSWAnnotations$Filename, pattern = '_', n = 4)[, 3]
SSWAnnotations$Month <- substr(SSWAnnotations$DATE, start = 5, stop = 6)
SSWAnnotations$Day <- substr(SSWAnnotations$DATE, start = 7, stop = 8)
SSWAnnotations$Month_Day <- paste(SSWAnnotations$Month, SSWAnnotations$Day, sep = '_')
SSWAnnotations$Hour <- substr(str_split_fixed(SSWAnnotations$Filename, pattern = '_', n = 4)[, 4],1,2)

# Merge the species tally data with Ithaca wather data based on Month_Day
SSWAnnotationsMergedDF <- merge(SSWAnnotations, IthacaSubset, by = 'Month_Day')

# Display the first few rows and the number of rows in SSWAnnotations
head(SSWAnnotationsMergedDF)
nrow(SSWAnnotationsMergedDF)

# Get unique values in the 'Species.eBird.Code' column
unique(SSWAnnotationsMergedDF$Species.eBird.Code)

# Create a table to look at the number of observations by species
table(SSWAnnotationsMergedDF$Species.eBird.Code)

# Subset based on your species of interest --------------------------------

# Subset SSWAnnotations for a specific species (e.g., "blujay")
SSWAnnotationsSingleSpecies <- subset(SSWAnnotationsMergedDF, Species.eBird.Code == "blujay")

# NOTE: the code below shows how you can subset for two species of interest
# SSWAnnotationsSingleSpecies <- subset(SSWAnnotationsMergedDF, Species.eBird.Code == "blujay"|Species.eBird.Code == "norcar")

# Display the first few rows of the subset
head(SSWAnnotationsSingleSpecies)

# Count the occurrences of the species by Month_Day
SSWAnnotationsSingleSpeciesTally <-  
  SSWAnnotationsSingleSpecies %>% count(Month_Day, Species.eBird.Code, .drop = FALSE)

SSWAnnotationsSingleSpeciesTally$n <- SSWAnnotationsSingleSpeciesTally$n/60

# Merge the species tally data with IthacaSubset based on Month_Day
SSWAnnotationsSingleSpeciesTallyMergedDF <- merge(SSWAnnotationsSingleSpeciesTally, IthacaSubset, by = 'Month_Day')


gghistogram(data=SSWAnnotationsSingleSpeciesTallyMergedDF,x='n')

# Create scatter and boxplots for 'TMAX' and 'n' (count) by Month
ggscatter(data = SSWAnnotationsSingleSpeciesTallyMergedDF, x = 'TMAX', y = 'n', facet.by ='Species.eBird.Code' )
ggboxplot(data = SSWAnnotationsSingleSpeciesTallyMergedDF, x = 'Month', y = 'n', facet.by ='Species.eBird.Code')

