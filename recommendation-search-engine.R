#loading the required packages in R session
require(SparkR)
require(tm)
require(SnowballC)

#setting up the Spark session
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#setting up file paths
data_file <- "input/plot_summaries.txt"
vector_file <- "plot_summary.RData"
corpus_file <- "summary_corpus.RData"

#read the data from "plot_summaries.txt" and save the data into a file
if (!file.exists(vector_file)){
  plot_summary <- read.delim(data_file, header=FALSE, sep = "\t", quote="")
  plot_summary$V2 <- as.character(plot_summary$V2)
  saveRDS(plot_summary, "plot_summary.RData")
  print("Saved!")
}
#reading the saved data.
plot_summary <- readRDS("plot_summary.RData")

if(!file.exists(corpus_file)){
  #getting the plot summaries into another dataframe
  summary_data <- plot_summary$V2
  
  #creating VCorpus for stemming by removing punctuations, stopwords and numbers, and strip whitespaces.
  addspace <- content_transformer(function(x, pattern) {
    return(gsub(pattern, " ", x))
  })
  summary_corpus <- VCorpus(VectorSource(summary_data))
  summary_corpus <- tm_map(summary_corpus, removePunctuation)
  summary_corpus <- tm_map(summary_corpus, removeNumbers)
  summary_corpus <- tm_map(summary_corpus, removeWords, stopwords("english"))
  summary_corpus <- tm_map(summary_corpus, content_transformer(tolower))
  summary_corpus <- tm_map(summary_corpus, stripWhitespace)
  
  #stemming the documents
  summary_corpus <- tm_map(summary_corpus, stemDocument)
  summary_dtm <- DocumentTermMatrix(summary_corpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE), stopwords = TRUE))
  saveRDS(summary_dtm, "summary_corpus.RData")
  print("Saved!")
}
summary_dtm <- readRDS("summary_corpus.RData")

# summary_dtm$dimnames$Docs <- as.numeric(summary_dtm$dimnames$Docs)
# summary_dtm$dimnames$Terms <- as.character(summary_dtm$dimnames$Terms)
# summary_dtm$dimnames$Terms <- gsub("[^[:alpha:] ]","", summary_dtm$dimnames$Terms)


