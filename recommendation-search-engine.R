require(SparkR)
sc <- sparkR.init()
sqlContext <- sparkRSQL.init(sc)

#read the data from "plot_summaries.txt" and save the data into a file
data_file <- "/Users/pradhanmanva/Downloads/R for DS/Project2/recommendation-search-engine/input/plot_summaries.txt"
saved_data_file <- "/Users/pradhanmanva/Downloads/R for DS/Project2/recommendation-search-engine/plot_summary.RData "
if (!file.exists(saved_data_file)){
  plot_summary <- read.delim(data_file, header=FALSE, sep = "\t", quote="")
  plot_summary$V2 <- as.character(plot_summary$V2)
  saveRDS(plot_summary, "plot_summary.RData")
}
plot_summary <- readRDS("plot_summary.RData")