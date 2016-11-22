########################################################################################
# Bibliometric Analysis of Circular Economy (CE), Green Economy (GE), Bioeconomy (BE)
# discourses for a systematic comparison between the content of research on these topics
# Script authors: N. Droste (nils.droste@ufz.de), D. D'Amato (dalia.damato@helsinki.fi)
# an adaptation of the nails project source code: http://nailsproject.net/ 
########################################################################################

# -1 setting wd, loading packages, preps -----------------------------------------------

setwd("/home/droste/Dropbox/Dokumente/doctorate/green economy/Damatoetal2016/data")

# Loading libraries
library(splitstackshape)
library(reshape)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(grid)
library(igraph)
library(knitr)
#library(ldatuning)
library(SnowballC)
library(tm)
library(lda)
library(LDAvis)
library(rworldmap)

# Session Info
sessionInfo()

# Source helper functions
source("helperFunctions.R")

# 0 data preparation / cleaning --------------------------------------------------------
########################################################################################
# Input folder should contain full records and citations downloaded from
# Web of knowledge in tab-delimited (UTF-16) format. Working directory should
# contain the list of fieldtags for naming the variables (fieldtags.csv).
########################################################################################

  # # Check whether to use topic modeling
  # enableTM <- TRUE

# Load variable names
fieldtags <- read.csv(paste(getwd(), "/fieldtags.csv", sep=""), header = T, sep = ";")
filelist <- list()
literatureList <- list()

#Loop over three topics
for (i in c("CE", "GE", "BE")){
  
  # List files in input folder
  filelist[[i]] <- list.files(paste(getwd(), "/input/", i, sep=""), full.names = T)
  
  # Load files in the input folder and merge into a single file
  for (file in filelist[[i]]) {
      literature <- read.delim2(file, header = T,
                                fileEncoding = "UTF-16", row.names = NULL,
                                quote = "", stringsAsFactors=FALSE )
      # Fix misplaced column names
      data.names <- names(literature)[2:length(names(literature))]
      literature <- literature[, 1:(ncol(literature) - 1)]
      names(literature) <- data.names
      # Merge data
      literatureList[[i]] <- rbind(literatureList[[i]], literature)
    }
  
  # Create and add id variable
  id <- c(1:nrow(literatureList[[i]]))
  literatureList[[i]] = cbind(as.data.frame(id), literatureList[[i]])
  
  # Cleaning data
  
  # Fix variable names
  tags <- names(literatureList[[i]])       # Extract column names
  # Match column names (acronyms) with full column names
  fields <- as.character(fieldtags$field[match(tags, fieldtags$tag)])
  fields[is.na(fields)] <- tags[is.na(fields)]     # Throws warnings but seems to be working
  fields <- gsub(" ", "", fields)         # Remove spaces
  
  # Change literature column names and fix weird names
  names(literatureList[[i]]) <- fields
  names(literatureList[[i]])[names(literatureList[[i]]) == "KeywordsPlus\xfc\xbe\x8e\x86\x84\xbc"] <- "KeywordsPlus"
  names(literatureList[[i]])[names(literatureList[[i]]) == "PublicationType(conference,book,journal,bookinseries,orpatent)"] <- "PublicationType"
  names(literatureList[[i]])[names(literatureList[[i]]) == "29-CharacterSourceAbbreviation"] <- "SourceAbbreviation"
  names(literatureList[[i]])[names(literatureList[[i]]) == "DigitalObjectIdentifier(DOI)" ] <- "DOI"
  
  #Format Data
  literatureList[[i]]$AuthorFullName <- toupper(literatureList[[i]]$AuthorFullName)
  literatureList[[i]]$AuthorFullName <- gsub("'", "", literatureList[[i]]$AuthorFullName)
  literatureList[[i]]$AuthorFullName <- gsub('"', "", literatureList[[i]]$AuthorFullName)

  literatureList[[i]]$AuthorKeywords <- tolower(literatureList[[i]]$AuthorKeywords)
  literatureList[[i]]$AuthorKeywords <- gsub("'", "", literatureList[[i]]$AuthorKeywords)
  literatureList[[i]]$AuthorKeywords <- gsub('"', "", literatureList[[i]]$AuthorKeywords)
  
  literatureList[[i]]$KeywordsPlus <- tolower(literatureList[[i]]$KeywordsPlus)
  literatureList[[i]]$KeywordsPlus <- gsub("'", "", literatureList[[i]]$KeywordsPlus)
  literatureList[[i]]$KeywordsPlus <- gsub('"', "", literatureList[[i]]$KeywordsPlus)
  
  literatureList[[i]]$YearPublished <- as.numeric(as.character(literatureList[[i]]$YearPublished))
  
  literatureList[[i]]$DocumentTitle <- gsub("'", "", literatureList[[i]]$DocumentTitle)
  literatureList[[i]]$DocumentTitle <- gsub('"', "", literatureList[[i]]$DocumentTitle)
  
  literatureList[[i]]$SubjectCategory <- tolower(literatureList[[i]]$SubjectCategory)
  literatureList[[i]]$SubjectCategory <- gsub("'", "", literatureList[[i]]$SubjectCategory)
  literatureList[[i]]$SubjectCategory <- gsub('"', "", literatureList[[i]]$SubjectCategory)
  
  literatureList[[i]]$CitedReferences <- gsub("'", "", literatureList[[i]]$CitedReferences)
  literatureList[[i]]$CitedReferences <- gsub('"', "", literatureList[[i]]$CitedReferences)
  literatureList[[i]]$CitedReferences <- toupper(literatureList[[i]]$CitedReferences)
  literatureList[[i]]$CitedReferences <- gsub("DOI DOI", "DOI", literatureList[[i]]$CitedReferences)
  
  literatureList[[i]]$TimesCited <- as.numeric(as.character(literatureList[[i]]$TimesCited))
  
  literatureList[[i]]$DOI <- toupper(literatureList[[i]]$DOI)
  
}

rm(list = c("data.names", "fields", "fieldtags", "file", "filelist", "i", "id", "literature", "tags"))

# check number of obs
#lapply(literatureList, nrow)

# 1 write seperate csv files for different analytical topics ---------------------------------

##LOCATIONS
for (i in c("CE", "GE", "BE")){

  # Extract cities and countries
  literatureList[[i]]$Locations <- sapply(literatureList[[i]]$AuthorAddress, get_location)
  
  # Split locations by ";
  locationList <- unlist(lapply(literatureList[[i]]$Locations,
                                function(x) strsplit(x, ";")))
  
  locations <- data.frame(location = locationList)        # Create data frame
  locations$location <- as.character(locations$location)  # To chararcter type
  locations$city <- gsub(",.*", "", locations$location)   # Remove country from location
  locations$country <- gsub(".*,", "", locations$location) # Remove city from location
  
  # Save locations
  write.table(locations, paste(getwd(), "/output/", i, "/locations_", i, ".csv", sep=""),
              sep = ";", row.names = F, qmethod = "double")
  
  #remove temp data
  rm(list = c("locations", "locationList"))
}

##KEYWORDS
for (i in c("CE", "GE", "BE")){
  # Create a new data frame, where each keyword is in a separate row.
  
  literatureByKeywords <- subset(literatureList[[i]],
                                 select = c("AuthorKeywords", "id"))
  literatureByKeywords <- literatureByKeywords[
    !is.na(literatureByKeywords$AuthorKeywords),]
  literatureByKeywords <- literatureByKeywords[
    literatureByKeywords$AuthorKeywords != "", ]
  using_KeywordsPlus = FALSE
  
  if (nrow(literatureByKeywords) == 0) {
    literatureByKeywords <- subset(literatureList[[i]],
                                   select = c("KeywordsPlus", "id"))
    names(literatureByKeywords)[1] <- "AuthorKeywords"
    literatureByKeywords <- literatureByKeywords[
      !is.na(literatureByKeywords$AuthorKeywords),]
    literatureByKeywords <- literatureByKeywords[
      literatureByKeywords$AuthorKeywords != "", ]
    using_KeywordsPlus = TRUE
  }
  
  if (nrow(literatureByKeywords) > 0) {
    literatureByKeywords <- cSplit(literatureByKeywords,
                                   splitCols = "AuthorKeywords",
                                   sep = ";", direction = "long")
    literatureByKeywords <- literatureByKeywords[
      !is.na(literatureByKeywords$AuthorKeywords),]
    literatureByKeywords <- subset(literatureByKeywords,
                                   select = c("id", "AuthorKeywords"))
    literatureByKeywords <- merge(literatureByKeywords,
                                  subset(literatureList[[i]], select = -c(AuthorKeywords)),
                                  by = "id")
  }
  
  # Save file
  write.table(literatureByKeywords, paste(getwd(), "/output/", i, "/literature_by_keywords_", i, ".csv", sep=""),
              row.names = F, sep = ';', qmethod = "double") 
  
  #remove temp data
  rm(list = c("literatureByKeywords", "using_KeywordsPlus"))
}  


#SAVE LITERATURE FILE
for (i in c("CE", "GE", "BE")){
  # Save the literature as a single csv-file literature.csv.
  write.table(literatureList[[i]], paste(getwd(), "/output/", i, "/literature_", i, ".csv", sep=""),
              sep = ";", row.names = F, qmethod = "double")
}

# 2 Topic modelling ---------------------------------------------------------------------------------

for (i in c("CE", "GE", "BE")){  
    # Do topic modeling on abstracts using the lda libraries (adding them as a new column)
    source(paste(getwd(), "/topicmodel.R", sep = ""), chdir = T)
    
    # Add top topic to main document
    literatureList[[i]]$TopicModelTopic <- tfdDF$toptopic
    
    # Save the topic model topic descriptions
    write.table(topwords, paste(getwd(), "/output/", i, "/topicmodeltopics_", i , ".csv", sep=""),
                sep = ";", row.names = F, qmethod = "double")
    
    outDir = paste(getwd(), "/output/", i, "/topicmodelvis_", i , sep = "")
    
    # HTML output
    serVis(json, out.dir = outDir, open.browser = FALSE)
    
    # Freeing up memory
    rm(list = c("json", "outDir", "topwords", "tfdDF"))
  }

# 3 plotting -----------------------------------------------------------------------

## Publication years
  
yearPlot1 <- ggplot(literatureList[[1]], aes(YearPublished)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  ggtitle("CE") +
  xlab("Year") +
  ylab("Article count") +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(limits = c(1990, 2017))

yearPlot2 <- ggplot(literatureList[[2]], aes(YearPublished)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  ggtitle("GE") +
  xlab("Year") +
  ylab("Article count") +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(limits = c(1990, 2017))

yearPlot3 <- ggplot(literatureList[[3]], aes(YearPublished)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  ggtitle("BE") +
  xlab("Year") +
  ylab("Article count") +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(limits = c(1990, 2017))

yearPlot <- grid.draw(cbind(ggplotGrob(yearPlot1), ggplotGrob(yearPlot2), ggplotGrob(yearPlot3), size = "last"))
oldDir <- getwd()
setwd(paste(getwd(), "/output/plots/", sep = ""))
dev.print(file= "yearPlot.png", device=png, width=800, height= 350)
dev.off()
setwd(oldDir)
rm(list = c("yearPlot1", "yearPlot2", "yearPlot3"))

#KEYWORDS

keywordDFList = list()
keywordPlotList <- list()
for (i in c("CE", "GE", "BE")){
  #print(i)
  setwd(paste(getwd(), "/output/", i, sep = ""))
  
  keywordDFList[[i]] <- read.csv(paste("literature_by_keywords_", i, ".csv", sep = ""), sep = ';', stringsAsFactors=FALSE) 
  setwd(oldDir)
}
  
x1 <- keywordDFList[[1]] %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x1 <- as.data.frame(head(x1, n = 10))
keywordPlot1<- ggplot(x1, aes (reorder(x1[,1], x1[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("CE")

x2 <- keywordDFList[[2]] %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x2 <- as.data.frame(head(x2, n = 10))
keywordPlot2<- ggplot(x2, aes (reorder(x2[,1], x2[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("GE")

x3 <- keywordDFList[[3]] %>% group_by(AuthorKeywords) %>% summarize(freq = n()) %>% arrange(desc(freq))
x3 <- as.data.frame(head(x3, n = 10))
keywordPlot3<- ggplot(x3, aes (reorder(x3[,1], x3[,2]), freq)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(y = "Number of occurences", x = "Keywords") +
  ggtitle("BE")

keywordPlot <- grid.draw(cbind(ggplotGrob(keywordPlot1), ggplotGrob(keywordPlot2), ggplotGrob(keywordPlot3), size = "last"))
setwd(paste(getwd(), "/output/plots/", sep = ""))
dev.print(file= "keywordPlot.png", device=png, width=1000, height= 350)
dev.off()
setwd(oldDir)

#LOCATIONS
oldDir <- getwd()
keywordLocList = list()
for (i in c("CE", "GE", "BE")){
  #print(i)
  setwd(paste(getwd(), "/output/", i, sep = ""))
  keywordLocList[[i]] <- read.csv(paste("locations_", i, ".csv", sep = ""), sep = ';', stringsAsFactors=FALSE) 
  setwd(oldDir)
}

keywordLocList$CE$country=gsub("Peoples R China", "China", keywordLocList$CE$country)
keywordLocList$GE$country=gsub("Peoples R China", "China", keywordLocList$GE$country)
keywordLocList$BE$country=gsub("Peoples R China", "China", keywordLocList$BE$country)

keywordLocList$CE[grep("USA", keywordLocList$CE$country), "country"] <- "United States"
keywordLocList$CE[grep("Scotland|England|Wales", keywordLocList$CE$country), "country"] <- "United Kingdom"
keywordLocList$GE[grep("USA", keywordLocList$GE$country), "country"] <- "United States"
keywordLocList$GE[grep("Scotland|England|Wales", keywordLocList$GE$country, ignore.case=T), "country"] <- "United Kingdom"
keywordLocList$BE[grep("USA", keywordLocList$BE$country), "country"] <- "United States"
keywordLocList$BE[grep("Scotland|England|Wales", keywordLocList$BE$country), "country"] <- "United Kingdom"

keywordLocList$CE[grep("Guadeloupe", keywordLocList$CE$country), "country"] <- "France"
keywordLocList$GE[grep("U Arab Emirates", keywordLocList$GE$country), "country"] <- "United Arab Emirates"
keywordLocList$GE[grep("Trinid & Tobago|Trinidad & Tobago", keywordLocList$GE$country), "country"] <- 'Trinidad & Tobago'


keywordLocList$CE <- na.omit(keywordLocList$CE)
keywordLocList$GE <- na.omit(keywordLocList$GE)
keywordLocList$BE <- na.omit(keywordLocList$BE)

ce <- as.data.frame(table(keywordLocList$CE$country)); colnames(ce)[1] <- "country"
ge <- as.data.frame(table(keywordLocList$GE$country)); colnames(ge)[1] <- "country"
be <- as.data.frame(table(keywordLocList$BE$country)); colnames(be)[1] <- "country"

sort(table(keywordLocList$CE$country), dec=T)
sort(table(keywordLocList$GE$country), dec=T)
sort(table(keywordLocList$BE$country), dec=T)

location_CE <- joinCountryData2Map(ce, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE)
location_GE <- joinCountryData2Map(ge, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE) #somehow missing three from "Trinidad & Tobago"
location_BE <- joinCountryData2Map(be, joinCode = "NAME", nameJoinColumn = "country", verbose=TRUE)

setwd(paste(getwd(), "/output/plots/", sep = ""))

par(mai=c(0.1,0.1,0.1,0.1),xaxs="i",yaxs="i")
nPanels <- layout( cbind(c(1,2,3))
                   , heights=c(1,1,1)
                   , respect=F)

mapCountryData(location_CE, nameColumnToPlot="Freq", mapTitle="CE", addLegend=T , colourPalette="white2Black", catMethod='logFixedWidth')
#do.call( addMapLegend, c(location_CE , legendWidth=0.5, legendMar = 2))
#dev.print(file= "locMapCE.png", device=png, width=800, height= 350)

mapCountryData(location_GE , nameColumnToPlot="Freq", mapTitle="GE", addLegend=T , colourPalette="white2Black", catMethod='logFixedWidth')
#dev.print(file= "locMapGE.png", device=png, width=800, height= 350)

mapCountryData(location_BE, nameColumnToPlot="Freq", mapTitle="BE", addLegend=T , colourPalette="white2Black", catMethod='logFixedWidth')
#dev.print(file= "locMapBE.png", device=png, width=800, height= 350)

dev.print(file= "locationsPlot.png", device=png, width=1200, height= 2400, res=300)
dev.off()

rm(i)