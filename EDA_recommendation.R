##Performing epxloratory data analysis on the dataset
#loading the required packages in R session
require(tm)
require(SnowballC)
require(wordcloud)
require(SparkR)
require(dplyr)
require(tidytext)
require(stringr)
require(stringi)


setwd(getwd())

#Reading the input data file
plot_summary <- read.delim("http://utdallas.edu/~rxk164330/MovieSummaries/plot_summaries.txt", header=FALSE, sep = "\t", quote="")
names(plot_summary)<-c("Id","Summary")
plot_summary$Id <- as.integer(plot_summary$Id)
plot_summary$Summary <- as.character(plot_summary$Summary)

#Plotting the Frequency of words before pre-processing
count <- sapply(strsplit(plot_summary$Summary, " "), length)
plot(count, main = "Frequency of Words in the dataset",
     xlab = "Index of Word", 
     ylab = "Frequency")

#Plotting a graph for words with frequency between 0 to 1500
count_range <- count[count < 1500]
count_range <- count_range[count_range> 0]
hist(count_range, main = "Histogram of Words with frequency 0-1500",
     xlab = "Frequency", 
     ylab = "Number of Records")

#Examining the % records covered by range of words
percent <- function(x) {
  return (length(x) * 100 / nrow(plot_summary))
}
count_bin <- tapply(count, cut(count, 10, include.lowest = T), percent)
count_bin

#Finding the number of unique words in the data
words <- unlist(strsplit(plot_summary$Summary," "))
length(unique(words))

#Function to get a clean corpus from the given data
getCorpusFromDocument <- function(documents) {
  corpus <- VCorpus(VectorSource(documents)) %>%
    tm_map(stemDocument) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(stripWhitespace)
  return(corpus)
}

#Applying function to clean pre-process the corpus
data_preprocessed<-getCorpusFromDocument(plot_summary$Summary)

#Creating the Term Document Matrix
dtm <- as.matrix(TermDocumentMatrix(data_preprocessed))
m <- as.matrix(dtm)
word_freq <-sort(rowSums(m),decreasing=TRUE)
dtm_df <- data.frame(word = names(word_freq),freq=word_freq)
head(dtm_df)


#Creating a word cloud
set.seed(1234)
wordcloud(words = dtm_df$word, freq = dtm_df$freq, min.freq = 10,
          max.words=50, random.order=FALSE, 
          colors=brewer.pal(6, "Dark2"))

#Plotting the Frequency of words after pre-processing
count <- nrow(dtm_df$word)
plot(dtm_df$freq ~ dtm_df$word, main = "Frequency of Words in the dataset",
     xlab = "Index of Word", 
     ylab = "Frequency")
