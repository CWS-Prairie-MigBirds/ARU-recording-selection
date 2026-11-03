######Boreal Monitoring Strategy Recording Selection Workflow#####

# Script adapted from:Steven Van Wilgenburg's Landbird Recording Selection Workflow
# Script adapted by: Janine McManus janine.mcmanus@ec.gc.ca
# Script adapted further by: Barry Robinson barry.robinson@ec.gc.ca
# Script adapted further by: Judith Toms judith.toms@ec.gc.ca

# Load Required Packages

# TrillR is not yet on CRAN, use devtools to install it from Github
#install.packages("devtools")
#library(devtools)
#devtools::install_github("deanrobertevans/TrillR")

library(dplyr)
library(TrillR) 
library(seewave)
library(tuneR)
library(suncalc)
library(hms)
library(tidyverse)


# We will select the dates and times based on good weather at one station, and then copy recordings with the same
# dates and times for all other stations in the PSU.
# Assumes that recordings are stored with each PSU as a directory, and each site within a PSU as a separate
# sub-directory within that PSU directory (PSU = grid for boreal)
# Enter parent directory that contains all the recordings for all sections
setwd("D:/2026/NCN/")
parentdir <- "D:/2026/NCN/"

# This function will open recordings in audacity directly from this script. Include the file path to where 
# Audacity is installed on your computer. Best spectrogram settings for birds can be found here:
# https://sk.birdatlas.ca/wp-content/uploads/2018/12/Audacity-Specs.pdf
setWavPlayer(shQuote("C:/Program Files/Audacity/Audacity.exe"))

# Create new folders for day and nighttime recordings since they have to be uploaded to WildTrax separately
# temporarily blocked out with hashtags as I already created these directories
# dir.create("D:/Wildtrax Upload")
new.folder.10M1SPT <- "D:/Wildtrax Upload/10M1SPT"
new.folder.3M1SPT <- "D:/Wildtrax Upload/3M1SPT"
new.folder.dusk <- "D:/Wildtrax Upload/DUSK"
new.folder.nocturnal <- "D:/WildTrax Upload/NOCTURNAL"
# dir.create(new.folder.10M1SPT)
# dir.create(new.folder.3M1SPT)
# dir.create(new.folder.dusk)
# dir.create(new.folder.nocturnal)


# Enter the PSU ID you are starting with
# NOTE: If more than one PSU in the PSU has been survey, each PSU needs to be within a separate folder

PSU <- "734--323"

# Set working directory to first station folder within the PSU folder
#setwd(list.dirs(paste(parentdir, PSU, sep = "/"), recursive = F)[1])
setwd(list.dirs(paste(parentdir, PSU, sep = "/"))) ### removed ", recursive = F)[1]" because I knoe there are no subdirectories

# Need to download and Sox - Sound Exchange for TrillR to work. Change the file path to where you have it saved.
# Note: do not download the normal executable file from the main page. Instead go to the files tab, then the Sox folder,
# the most recent version, download the zip file, and unzip directly into a program folder. This can be installed and
# run directly without admin priviledges.
# https://sourceforge.net/projects/sox/

setsox.exe("C:/sox-14-4-2/sox.exe")


# Get .wav data
# The following function creates a dataframe with file directory, location, data/time, and julian date.
# Specify the start and end dates of the selection you would like to do (e.g. start.date="2019-06-01" to end.date="2019-06-30")
# HERE we are limiting to dates within the primary breeding period for most songbirds in Canada BCR 11. 
# Can alter and add draws for other spp groups later.

data <- get.wavs(start.date = "2026-05-28",end.date = "2026-07-07") 
# Corrupted files introduce NA's into location category that will mess up future steps. Remove any NAs.

#Parse out location from the filename
data$UniqueID<- substr(data$basename, 1, 12) ### change last bit depending on length of name
data <- data[!is.na(data$location),]

# Inspect data for anything out of the ordinary
summary(data)
hist(data$JDay)


###############################################################

# Specify the location of the ARU
# Import location table and merge with data table
loc <- read.csv("D:/2026/locations.csv") ### double check that dates were entered correctly (YYYY--MM--DD). 
#loc <-subset(loc, Backed_up=="Yes")

############## USE DEPLOYMENT DATE to get rid of recordings where the ARUs were left recording in  BOX

loc$JDay <- as.Date(loc$Deploy.Date)
loc$JDay <- as.numeric(format(loc$JDay, "%j"))


data <- mergelocations(data = data, locationdata = loc, locationname = "UniqueID", 
                       Latitude = "Latitude", Longitude = "Longitude")

# Calculate sunrise and sunset for that location
data <- getSunCalcs(data, calc = c("sunrise","sunset"), doParallel=T)

##### Set start.date based on MINIMUM date to draw based on DEPLOYMENT DATE for the PSU from which recordings are being drawn
start <- (loc[loc$UniqueID %in% data$location,])$JDay
### ABOVE MIGHT NOT WORK FOR FALL DEPLOYMENTS ???



#############################################################################################
# Create Categories

# For the boreal monitoring program we need one 10-minute recording and five 3-minute recordings split into early and late
# EE = early in year, early in day; EM = early in year, mid-morning; EL = early in year, late in morning;
# LE = late in year, early in morning; LM = late in year, mid-morning; LL = late in year, late in morning.
# EE will be kept as 10 min recording, all others will be trimmed to 3-minute recordings for transcription.
# For night recordings, we will randomly select three recordings from ~dusk. 
# I've added a little wiggle room in the minutes to account for small variations in ARU programming.  

data <- subset(data, JDay>= start) ### subsetting to remove days that might have recorded silence etc as result of ARU being turned on prior to deployment

#### custom selection functions used later in the script.
# Custom function used below to simplify extraction of date-time information
extract.date.time <- function(filepath.n, Draw = "Morning") {
  # Extracts date.time info from recording filepath, for use in pattern. Will only need to provide correct
  # recording filepath number
  # Default is Morning, can specify Dusk
  if (Draw == "Morning") {
    name <- MorningDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  else if (Draw == "Dusk"){
    name <- DuskDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  else if (Draw == "Nocturnal"){
    name <- NocturnalDraw$file.path[filepath.n]
    date.time <- substr(name, nchar(name) - 18, nchar(name) - 4)
  }
  return(date.time)
}


##### REVISED Custom Selection function that allows for some ARU synchronization errors between ARUs in the same PSU. Here I padded the time by +-10 minutes
select_recordings <- function(draw, time_of_day = "Morning", initial_file_index, target_folder){
  continue_looping <- TRUE
  counter <- initial_file_index

  while(continue_looping == TRUE){

    print(paste("playing recording #", counter))
    play(draw$file.path[counter])

    answer <- tolower(readline(
      prompt = "Is the recording good enough to transcribe?? (y or n, or x to escape the function)"
    ))

    if(answer == "y"){

      print(paste("using recording #", counter))

      # Early 1 - After you find a good recording (first one),
      # replace correct file number in variable 'recording_to_use_...' below
      # recording_to_use_early_deployment <- 2

      date.time <- extract.date.time(counter, time_of_day)

      # Copy all files within ±2 minutes of selected recording
      target_time <- as.POSIXct(
        date.time,
        format = "%Y%m%d_%H%M%S",
        tz = "UTC"
      )

      file_times <- as.POSIXct(
        sub(".*?(\\d{8}_\\d{6}).*", "\\1", section.list$file.path),
        format = "%Y%m%d_%H%M%S",
        tz = "UTC"
      )

      files <- section.list$file.path[
        abs(as.numeric(difftime(file_times, target_time, units = "mins"))) <= 10
      ]

      files  # check to make sure the list looks correct

      file.copy(files, target_folder)

      print("recordings copied over")

      continue_looping <- FALSE

    } else if(answer == "n"){

      counter <- counter + 1

    } else if(answer == "x"){

      continue_looping <- FALSE

      print("loop stopped, if you ran out of recordings, re-draw above before running this function again")

    } else {

      print("key(s) entered are not accepted, only y, n, or x accepted")

    }
  }

  return(counter)
}


# Step 5. Create temporal categories for selection
# For the boreal monitoring program we use one 10-minute recording and five 3-minute recordings in the morning, split into early and late blocks of the deployment. We also transcribe 3 dusk recordings.

#EE = First half of recordings and an hour before sunrise to an hour after sunrise
#EM = First half of recordings and an hour after sunrise to three hours after sunrise.
#EL = First half of recordings and three hours after sunrise to five hours after sunrise.

#LE = late in deployment, early in morning
#LM = late in deployment, mid-morning
#LL = late in deployment, late in morning.

#EE will be kept as 10 min recording, all others will be trimmed to 3-minute recordings for transcription. For dusk/night recordings, we will randomly select three recordings across the entire deployment window. 

#There is a little wiggle room in the minutes below to account for small variations in ARU programming.  


data <- data  %>% mutate(category= NA) %>% 
  mutate(start.date=min(JDay),end.date= median(JDay),start.time=as_hms(sunrise-(65*60)),end.time=as_hms(sunrise +(50*60))) %>% 
  categorize("EE",start.date,end.date,start.time,end.time) %>% 
  
  mutate(start.date=min(JDay),end.date= median(JDay),start.time=as_hms(sunrise + (51*60)),end.time=as_hms(sunrise +(2.5*60*60))) %>% 
  categorize("EM",start.date,end.date,start.time,end.time) %>% 
  
  mutate(start.date=min(JDay),end.date= median(JDay),start.time= as_hms(sunrise+(2.51*60*60)),end.time= as_hms(sunrise+(5*60*60)))%>% 
  categorize("EL",start.date,end.date,start.time,end.time) %>%
  
  mutate(start.date=median(JDay) +1, end.date= max(JDay),start.time=as_hms(sunrise -(65*60)),end.time=as_hms(sunrise +(50*60))) %>% 
  categorize("LE",start.date,end.date,start.time,end.time) %>% 
  
  mutate(start.date=median(JDay) + 1, end.date= max(JDay),start.time=as_hms(sunrise + (51*60)),end.time=as_hms(sunrise +(2.5*60*60))) %>% 
  categorize("LM",start.date,end.date,start.time,end.time) %>% 
  
  mutate(start.date= median(JDay) + 1, end.date= max(JDay),start.time= as_hms(sunrise+(2.51*60*60)),end.time= as_hms(sunrise+(5*60*60)))%>% 
  categorize("LL",start.date,end.date,start.time,end.time) %>%
  
  mutate(start.date=min(JDay),end.date= max(JDay),start.time=as_hms(sunset - (40*60)),end.time=as_hms(sunset+(40*60))) %>% 
  categorize("Dusk",start.date,end.date,start.time,end.time) %>%
  
  mutate(start.date=min(JDay), end.date=max(JDay), start.time=as_hms(sunset+(40*60)), end.time=as_hms(sunrise-(65*60))) %>%
#  categorize("NOCTURNAL",start.date,end.date,start.time,end.time) %>%
   categorize("Nocturnal",start.date,end.date,start.time,end.time) %>%

  select(-start.date, -end.date, -start.time, -end.time)



table(data$category)


# Remove any NAs.
dataselection <- data[!is.na(data$category),]
table(data$category)

# Step 6. Subset recordings between morning and dusk/nocturnal
dusk <- subset(data, category=="Dusk")
morn <- subset(data, category!="Dusk" & category!="Nocturnal")
noct <- subset(data, category=="Nocturnal")


# Step 7. Draw the recordings
# Randomly sample a number of recordings from morning and dusk. 6 samples seems to be a good number to make sure you have at least one good weather recording. In this case we filter EE recordings to only those that are 10min long.
# Take a larger dusk sample since you need three recordings from the draw

MorningDraw <- morn %>% filter(case_when(category == "EE" ~ file.duration > 599,
                   T ~ file.duration > 1)) %>%  group_by(category) %>% sample_n(6)


DuskDraw <- dusk  %>% sample_n(12)

NocturnalDraw <- noct %>% sample_n(12)



knitr::opts_knit$set(root.dir = paste(parentdir, PSU, sep = "/"))

section.list <- get.wavs(start.date = "2026-05-28",end.date = "2026-07-07")

# Corrupted files introduce NA's into location category that will mess up future steps. Remove any NAs.
# first a check... 
section.list[!is.na(section.list$location),] ### A check before running code below in case there is a deta entry or coding error producing inapprorpiate NAs

section.list <- section.list[!is.na(section.list$location),]


# Step 9. Run the selection

#We recommend having a file window open to the target folder, and after each category selection inspect that the recordings have copied properly. There may be situations where one station in the PSU stopped recording early, where clock/lat longs were programmed differently between units in a PSU, or two stations accidentally were entered with the same name in the field. It's best to trouble shoot these issues as you go, and for instances where it's just a single station that requires a different recording, you should manually copy over the nearest matching time to the draw that is good quality.

#EE selection
file_used_EE <- select_recordings(MorningDraw, "Morning", initial_file_index = 1, target_folder = new.folder.10M1SPT)

#EL selection
file_used_EL <- select_recordings(MorningDraw, "Morning", initial_file_index = 7, target_folder = new.folder.3M1SPT)

#EM selection
file_used_EM <- select_recordings(MorningDraw, "Morning", initial_file_index = 13, target_folder = new.folder.3M1SPT)

#LE selection
file_used_LE <- select_recordings(MorningDraw, "Morning", initial_file_index = 19, target_folder = new.folder.3M1SPT)

### NA

#LL selection
file_used_LL <- select_recordings(MorningDraw, "Morning", initial_file_index = 25, target_folder = new.folder.3M1SPT)


#LM selection
file_used_LM <- select_recordings(MorningDraw, "Morning", initial_file_index = 31, target_folder = new.folder.3M1SPT)


#Dusk1 selection
dusk1 <- select_recordings(DuskDraw, "Dusk", initial_file_index = 1, target_folder = new.folder.dusk)

#Dusk2 selection
dusk2 <- select_recordings(DuskDraw, "Dusk", initial_file_index = dusk1 + 1, target_folder = new.folder.dusk)

#Dusk3 selection
dusk3 <- select_recordings(DuskDraw, "Dusk", initial_file_index = dusk2 + 1, target_folder = new.folder.dusk)


#Nocturnal selection
noct1 <- select_recordings(NocturnalDraw, "Nocturnal", initial_file_index = 1, target_folder = new.folder.nocturnal)

#Nocturnal2 selection
noct2 <- select_recordings(NocturnalDraw,"Nocturnal", initial_file_index = noct1+1, target_folder = new.folder.nocturnal)

#Nocturnal3 selection
noct3 <- select_recordings(NocturnalDraw,"Nocturnal", initial_file_index = noct2+1, target_folder = new.folder.nocturnal)

#Nocturnal4 selection
noct4 <- select_recordings(NocturnalDraw, "Nocturnal", initial_file_index = noct3+1, target_folder = new.folder.nocturnal)

#Nocturnal5 selection
noct5 <- select_recordings(NocturnalDraw, "Nocturnal", initial_file_index = noct4+1, target_folder = new.folder.nocturnal)


##### FINALLY, we need to make sure that the recordings for upload are the correct length (we won't upload a 10 minute recording for 3 minute of processing. After selecting files for ALL PSUs, then use code below to truncate recordings

library(seewave)
library(tuneR)


#### shorten the 3 minute DAWN selection
setwd("D:/Wildtrax Upload/3M1SPT/") #### set the working directory to the location of recordings on SD card
startingDir<-"D:/Wildtrax Upload/3M1SPT/" #### location of where recordings names will be changed

filez<-list.files(startingDir,pattern=".wav") # create vector to reference the file names   
head(filez)

#### set the duration of the recording you want to output
minutes=3
dur=60*minutes 


### Read in the wav files and store in list
### only reads in the duration specified by dur=

wavs<-list()
for (i in 1:length(filez)){
	wavs[[i]]<-readWave(filez[i], from = 0, to = dur,units = c("seconds"))

}

wavs[1]

##### Output the shortened wav files
### create subdirectory to store the output files to upload to wildtrax
#path<-getwd()
#newfolder <- "Upload_to_Wildtrax"
#newpath <- file.path(dirname(path), newfolder)
#setwd(newpath) #### set the working directory to the location of recordings on SD card
#for (i in 1:length(wavs)){
#	writeWave(wavs[[i]], filename=paste(filez[i], sep = ""))
#}

### NOTE: This version OVERWRITES the 10 minute files wil the reduce duration files. THEREFORE, make copies of the originals in a folder to be used for upload to WildTrax
path<-getwd()
for (i in 1:length(wavs)){
	writeWave(wavs[[i]], filename=paste(filez[i], sep = ""))
}


#### shorten the DUSK selection
setwd("D:/Wildtrax Upload/DUSK/") #### set the working directory to the location of recordings on SD card
startingDir<-"D:/Wildtrax Upload/DUSK/" #### location of where recordings names will be changed

filez<-list.files(startingDir,pattern=".wav") # create vector to reference the file names   
head(filez)

#### set the duration of the recording you want to output
minutes=3
dur=60*minutes 


### Read in the wav files and store in list
### only reads in the duration specified by dur=

wavs<-list()
for (i in 1:length(filez)){
	wavs[[i]]<-readWave(filez[i], from = 0, to = dur,units = c("seconds"))

}

wavs[1]

##### Output the shortened wav files
### NOTE: This version OVERWRITES the 10 minute files wil the reduce duration files. THEREFORE, make copies of the originals in a folder to be used for upload to WildTrax
path<-getwd()
for (i in 1:length(wavs)){
	writeWave(wavs[[i]], filename=paste(filez[i], sep = ""))
}


#### shorten the Nocturnal selection
setwd("D:/Wildtrax Upload/NOCTURNAL/") #### set the working directory to the location of recordings on SD card
startingDir<-"D:/Wildtrax Upload/NOCTURNAL/" #### location of where recordings names will be changed

filez<-list.files(startingDir,pattern=".wav") # create vector to reference the file names   
head(filez)

#### set the duration of the recording you want to output
minutes=3
dur=60*minutes 


### Read in the wav files and store in list
### only reads in the duration specified by dur=

wavs<-list()
for (i in 1:length(filez)){
	wavs[[i]]<-readWave(filez[i], from = 0, to = dur,units = c("seconds"))

}
wavs[1]

##### Output the shortened wav files
### NOTE: This version OVERWRITES the 10 minute files wil the reduce duration files. THEREFORE, make copies of the originals in a folder to be used for upload to WildTrax
path<-getwd()
for (i in 1:length(wavs)){
	writeWave(wavs[[i]], filename=paste(filez[i], sep = ""))
}
