#loading the required packages in R session
require(SparkR)
require(tm)
require(SnowballC)

#setting up the Spark session
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#setting up file paths
data_file <- "/Users/pradhanmanva/Downloads/R for DS/Project2/recommendation-search-engine/input/plot_summaries.txt"
saved_data_file <- "/Users/pradhanmanva/Downloads/R for DS/Project2/recommendation-search-engine/plot_summary.RData "

#read the data from "plot_summaries.txt" and save the data into a file
if (file.exists(saved_data_file)){
  plot_summary <- read.delim(data_file, header=FALSE, sep = "\t", quote="")
  plot_summary$V2 <- as.character(plot_summary$V2)
  saveRDS(plot_summary, "plot_summary.RData")
}
#reading the saved data.
plot_summary <- readRDS("plot_summary.RData")

#getting the plot summaries into another dataframe
summary_data <- plot_summary$V2

#creating VCorpus for stemming by removing punctuations, stopwords and numbers, and strip whitespaces.
addspace <- content_transformer(function(x, pattern) {
  return(gsub(pattern, " ", x))
})
summary_corpus <- VCorpus(VectorSource(summary_data))
summary_corpus <- tm_map(summary_corpus, addspace, "-")
summary_corpus <- tm_map(summary_corpus, addspace, "'")
summary_corpus <- tm_map(summary_corpus, removePunctuation)
summary_corpus <- tm_map(summary_corpus, removeNumbers)
summary_corpus <- tm_map(summary_corpus, removeWords, stopwords("english"))
summary_corpus <- tm_map(summary_corpus, content_transformer(tolower))
summary_corpus <- tm_map(summary_corpus, stripWhitespace)
