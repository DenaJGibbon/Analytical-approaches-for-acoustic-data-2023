# Load the libraries
library(dplyr)
library(ggpubr)
library(stringr)

# Data from: Sethi, Sarab, Ewers, Robert M, Jones, Nick, Picinali, Lorenzo, Orme, David, Sleutel, Jani, Shabrani, Adi, Zulkifli, Nursyamin, & Bernard, Henry. (2020). 
# Avifaunal and Herpetofaunal point counts with recorded acoustic data [Data set]. Zenodo. https://doi.org/10.5281/zenodo.3997172

# Part 1. Prepare the data --------------------------------------------------------
# Read in data from point counts
BiodiversityDataFrame <- read.csv('data/BiodiversityAnalysis/BiodiversityDataFrameCountDFAddTime.csv')

# First let's look at the structure of the data
# NOTE: the 'count' variable is the number of unique species identified
head(BiodiversityDataFrame)

# Let's see what kind of land-use types there are
table(BiodiversityDataFrame$Site)

# Part 2. Data exploration --------------------------------------------------------

# Now let's see how many point counts there were
table(BiodiversityDataFrame$Point_count_ID)

# Wow that is a lot!
unique(BiodiversityDataFrame$Point_count_ID)

# Let's check the length
length(unique(BiodiversityDataFrame$Point_count_ID))

# Univariate data exploration
gghistogram(data=BiodiversityDataFrame,x='count')

# Plot by site
ggboxplot(data=BiodiversityDataFrame,x='Site',y='count', fill='Site')


# Data exploration for early morning hours --------------------------------
BiodiversityDataFrameAMOnly <-
  BiodiversityDataFrame[str_detect(BiodiversityDataFrame$Time,pattern = 'AM'),]

# And now we do by hour 
BiodiversityDataFrameAMOnly$Time <-
  as.numeric(str_split_fixed(BiodiversityDataFrameAMOnly$Time,pattern = ':',n=2)[,1])

# And focus on the daylight morning hours
BiodiversityDataFrameAMOnly <- subset(BiodiversityDataFrameAMOnly, Time > 05 & Time < 09)
  
# Now we can look at the number of species by time
ggboxplot(data=BiodiversityDataFrameAMOnly,x='Time',y='count')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Now let's look by time and site
ggboxplot(data=BiodiversityDataFrameAMOnly,x='Time',y='count',fill='Site')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Or only by site
ggboxplot(data=BiodiversityDataFrameAMOnly,x='Site',y='count',fill='Site')+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

