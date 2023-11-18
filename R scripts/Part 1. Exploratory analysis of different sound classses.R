# Load required packages --------------------------------------------------
# NOTE: You may need to install the packages first!
# install.packages('ggpubr')
# install.packages('seewave')
# install.packages('tuneR')
# install.packages('phonTools')
# install.packages('MASS')

library(ggpubr)
library(seewave)
library(tuneR)
library(phonTools)
library(MASS)

# If do randomization but want our results to be reproducible we use set.seed
set.seed(4)

# Part I.  Read in sound files and spectrograms ---------------------------

# We can easily read in the sound file using the following line of code
# NOTE: You put file.choose() in the console below to search for the full file path
LongSoundFile <- tuneR::readWave("/Users/denaclink/Desktop/RStudioProjects/Analytical Approaches for Acoustic Data Lab/S11_20180319_060002.wav")

# Now we can check the structure of the resulting .wav file
LongSoundFile@left # This returns the values of the waveform
LongSoundFile@samp.rate # This is the sample rate

# We can also read in the selection table made in Raven using the following code
SelectionTableName <- 'data/ExploratoryAnalysis/S11_20180319_060002.Table.1.selections.txt' 

SoundscapeTable <- read.delim(SelectionTableName,
           stringsAsFactors = F)

# Now we want to check the structure of the table
str(SoundscapeTable)
head(SoundscapeTable)

# This selection table has annotations of different call types
table(SoundscapeTable$Call.type)

# Let's do some exploratory data analysis
ggboxplot(data=SoundscapeTable,x='Call.type',y='Dur.90...s.')
ggboxplot(data=SoundscapeTable,x='Call.type',y='Freq.5...Hz.')
ggboxplot(data=SoundscapeTable,x='Call.type',y='Freq.95...Hz.')

# We can use the Raven selection table to isolate these particular sounds
# ListofWavs <- lapply(1:nrow(SoundscapeTable), function(x) cutw(LongSoundFile, from=SoundscapeTable$Begin.Time..s.[x],
#                                                  to=SoundscapeTable$End.Time..s.[x], output='Wave'))
# 
# # We now have a list of .wav files that were created from our Raven selection table. If we want we can save them to a local directory
# dir.create('data/ExploratoryAnalysis/SoundFiles') # This line creates a folder in your working directory
# 
# # This loop will write the shorter sound files to the directory indicated above
# for(x in 1:length(ListofWavs)){
#   writeWave(ListofWavs[[x]], 
#             filename= paste('data/ExploratoryAnalysis/SoundFiles','/',SoundscapeTable$Call.type[x],
#                             '_', x, '.wav',sep=''))
# }

# Now we can make a spectrogram. First lets read in the .wav file
FemaleGibbonFile <- readWave("data/ExploratoryAnalysis/SoundFiles/female.gibbon_2.wav")

# There are many different packages that you can use to create spectrograms; here are two
seewave::spectro(FemaleGibbonFile,flim=c(0,3))
phonTools::spectrogram(sound = FemaleGibbonFile@left,maxfreq=3000,windowlength = 24,fs=FemaleGibbonFile@samp.rate)

# Lets check out spectrograms of all the signals; first we read in the sound files
GreatArgusFile <- readWave("data/ExploratoryAnalysis/SoundFiles/argus_10.wav")
Bird1File  <- readWave("data/ExploratoryAnalysis/SoundFiles/bird1_9.wav")
Bird2File <- readWave("data/ExploratoryAnalysis/SoundFiles/bird2_20.wav")
Insect1File <- readWave("data/ExploratoryAnalysis/SoundFiles/insect1_24.wav")

# Now create the spectrograms
seewave::spectro(FemaleGibbonFile,flim=c(0,3))
seewave::spectro(GreatArgusFile,flim=c(0,3))
seewave::spectro(Bird1File,flim=c(0,3))
seewave::spectro(Bird2File,flim=c(0,3))
seewave::spectro(Insect1File,flim=c(3,6))

# Part 2. PCA: unsupervised classification ------------------------------------
# We read in the selection table again (this will be important later)
SelectionTableName <- 'data/ExploratoryAnalysis/S11_20180319_060002.Table.1.selections.txt' 

# This is commented out for now, but we will use it later
# SelectionTableName <- 'data/ExploratoryAnalysis/S11_20180319_060002.updatedTable.1.selections.txt' 

SoundscapeTable <- read.delim(SelectionTableName,
                              stringsAsFactors = F)

# Our Raven selection table also has some potentially useful features for distinguishing between call types
str(SoundscapeTable)
head(SoundscapeTable[,c(8:12)])

# First let's use the 'prcomp' function to calculate the PCA
BorneoCallTypes.pca <- 
 prcomp(SoundscapeTable[,c(8:11)])

# Check out the summary 
summary(BorneoCallTypes.pca)

# Check out the factor loadings
BorneoCallTypes.pca

# Create the default biplot (ugly)!
biplot(BorneoCallTypes.pca)

# Now we will make a prettier plot
# We combine PC1, PC2 and Call.type into
plot.for.BorneoCallTypes <-
  cbind.data.frame( scale(BorneoCallTypes.pca$x [,1:2]),
                   SoundscapeTable$Call.type)

# Add informative column names
colnames(plot.for.BorneoCallTypes) <-
  c("Dim.1", "Dim.2", "Call.type")

# Create the scatter plot
my_plot_BorneoCallTypes.pca <- 
ggscatter(data = plot.for.BorneoCallTypes,x = 'Dim.1',
          y = 'Dim.2',
          color  = 'Call.type')+ 
  ggtitle('Borneo call types PCA') + 
  xlab('PCA 1')+ylab('PCA 2')

# Show the plot
my_plot_BorneoCallTypes.pca


# Part 3. LDA: Supervised classification --------------------------------------
# How many observations?
NRowTable <- nrow(SoundscapeTable)
NRowTable

# What is 60% of the number of obs?
NTest <- round(NRowTable*0.6,0)
NTest

# Randomly sample 60% of obser for training
TrainSamples <- sample(seq(1:NRowTable),NTest,replace = FALSE)

# Subset only the training samples
DataTrain <- SoundscapeTable[TrainSamples,]

# Remove unnecessary columns
LDAdataTrain <- DataTrain[,c(8:12)]

# Isolate remaining samples as test data
DataTest <- SoundscapeTable[-TrainSamples,]

# Run LDA
lda.output <- MASS::lda(
  Call.type  ~ .,
  data=LDAdataTrain,
  center = TRUE,
  scale. = TRUE
)

# Let's inspect the output
lda.output

# Let's see what is going on in our data
ggboxplot(data=SoundscapeTable,x='Call.type',y='Dur.90...s.')
ggboxplot(data=SoundscapeTable,x='Call.type',y='Freq.5...Hz.')
ggboxplot(data=SoundscapeTable,x='Call.type',y='Freq.95...Hz.')

# You will get a warning message that the variables are collinear; this is OK
pairs(DataTrain[,c(8:11)])

# Now we use the LDA to predict the test observations
PredictedClass <- 
  predict(lda.output, DataTest[,c(8:11)])$class

# We create a confusion matrix containing actual and predicted labels
ct <-
  table(grouping =DataTest$Call.type, PredictedClass)

# Check structure of the table
ct

# Calculate total percent correct
percent <- sum(diag(prop.table(ct)))*100
percent

# We can use LDA as dimensionality reduction technique and create a biplot based on the LDA
# Here we use all the data
LDAdata <- SoundscapeTable[,c(8:12)]

# Run the LDA as before
fit.lda <- MASS::lda(Call.type ~ ., LDAdata)

# Use the 'predict' function to get LD1 and Ld2
class.lda.values <- predict(fit.lda)
head(class.lda.values$x)

# Combine the results into a new dataframe
newdata <- data.frame(class = LDAdata$Call.type, lda = class.lda.values$x)

# Check the output
head(newdata)

# Code to create the plot
my_plot_BorneoCallTypes.lda <-ggscatter(data = newdata,x = 'lda.LD1',
          y = 'lda.LD2', 
          color  = 'class',ellipse=T,ellipse.level = 0.95,ellipse.border.remove = F)+ 
  ggtitle('Borneo call types LDA')  + xlab("LD1") + ylab("LD2") 


my_plot_BorneoCallTypes.lda


# Part 4 (optional). Removing the insect  --------------------------------------------
# What happens if we remove the high-frequency sound type?
SoundscapeTable$Call.type <- as.factor(SoundscapeTable$Call.type)

# Use the subset function to remove the insect
SoundscapeTableNoInsect <- droplevels(subset(SoundscapeTable, Call.type !='insect1'))

# Run LDA
lda.output.noinsect <- MASS::lda(
  Call.type  ~ .,
  data=SoundscapeTableNoInsect[,c(8:12)]
)

# Let's inspect the output
lda.output.noinsect

# Use the 'predict' function to get LD1 and Ld2
class.lda.values.noinsect <- predict(lda.output.noinsect)

# Combine the results into a new dataframe
newdatanoinsect <- data.frame(class = SoundscapeTableNoInsect$Call.type, lda = class.lda.values.noinsect$x)

# Check the output
head(newdatanoinsect)

# Code to create the plot
my_plot_BorneoCallTypes.ldanoinsect <-ggscatter(data = newdatanoinsect,x = 'lda.LD1',
                                        y = 'lda.LD2', 
                                        color  = 'class',ellipse=T,ellipse.level = 0.95,ellipse.border.remove = F)+ 
  ggtitle('Borneo call types LDA')  + xlab("LD1") + ylab("LD2") 


my_plot_BorneoCallTypes.ldanoinsect



