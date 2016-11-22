
setwd("...")

#data("AssociatedPress", package="topicmodels")

library("ldatuning")
library("SnowballC")
library("tm")

dfCorpus <- Corpus(VectorSource(literature$Abstract)); dfCorpus[[1]]
dfCorpus = tm_map(dfCorpus, content_transformer(tolower))
dfCorpus = tm_map(dfCorpus, content_transformer(removePunctuation))
dfCorpus = tm_map(dfCorpus, content_transformer(removeNumbers))
dfCorpus = tm_map(dfCorpus, removeWords, stopwords("SMART"))
dfCorpus = tm_map(dfCorpus, stemDocument)
dfFrequencies = DocumentTermMatrix(dfCorpus) 
dfSparse = removeSparseTerms(dfFrequencies, 0.995)
narrativeSparse = as.data.frame(as.matrix(dfFrequencies))
colnames(narrativeSparse) = make.names(colnames(narrativeSparse))
narrativeSparse<-narrativeSparse[rowSums(narrativeSparse)>0,]

result <- FindTopicsNumber(
  narrativeSparse,
  topics = seq(from = 2, to = 60, by = 5),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4,
  verbose = TRUE
)
FindTopicsNumber_plot(result)
result$topics[which.max(result$Griffiths2004)]
