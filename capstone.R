library(data.table)
library(tm)
library(SnowballC)
library(parallel)
library(htmlwidgets)
library(tm.plugin.webmining)

if (!require("DT")) devtools::install_github("rstudio/DT")
library(DT)

cluster <- makeCluster(detectCores())
clusterEvalQ(cluster, library(tm))
options(mc.cores=4)


sampleFile <- function(fileIn, fileOut){
  set.seed(54321)
  
  conIn  <- file(fileIn, "r", blocking=FALSE)
  conOut <- file(fileOut, "w")
  
  while(length(line <- readLines(conIn, n=1)) > 0){
    sample.prob <- rbinom(1, 1, 0.5)
    writeLines(line, conOut)
    
    if(sample.prob==1){line <- readLines(conIn, n=99)}
  }  
  
  close(conIn)
  close(conOut)  
}



# Project only requires to cover US English langauge
corpus <- Corpus(DirSource("data/capstone/final/en_US/"), 
                readerControl = list(reader = readPlain, 
                                     language = "en_US", 
                                     encoding = "UTF-8",
                                     load = FALSE))

corpus.summary <- data.frame(Files=row.names(summary(corpus)),
                             Rows = sapply(corpus, function (x) {length(x$content)}),
                             Mean.Char = round(sapply(corpus, function (x) {mean(nchar(x$content))})),
                             Size = sapply(corpus, object.size))

datatable(corpus.summary, rownames=F, colnames = c("File Name", "Number of Rows", 
                                                   "Mean Characters", "Size"),
          options = list(dom="t",
                         autoWidth=FALSE,
                         columnDefs = list(list(width = "10%", className="dt-center",targets="_all"))),
          caption= "Table 1: Corpus Summary")


substr(corpus[[1]]$content[1:3],0,100)

names(corpus.summary) <- c("File Name", "Number of Rows")

https://www.cs.cornell.edu/courses/CS4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf
http://web.mit.edu/6.863/www/fall2012/readings/ngrampages.pdf

summary(corpus)

# Inspect first few rows of each of the documents
lapply(final, function (x) {x[[1]][1:3]})

lapply(corpus,function(x) {
        cbind(length(x$content), max(nchar(x$content))
        })

# Text is needs to be convereted to all lowercase to simplify
# Also based on the data source, looks like this data is not misspelled
# Not sure if I should do steming. It might improve accuracy of predictions by 
# Stop words removal might help predicive value. stopwords("en")
# Smoothing techniques. (assign a count of 1 to unseen n-grams; see Rule of succession)
# Could use Wordnet to find synonyms for words that we did not trained for when predicting the 
# next word. replaceWord can potentially do that 
# Maybe tagging with OpenNLP

# Remove profanity. Sourced from http://badwordslist.googlecode.com/files/badwords.txt
if (!file.exists("data/capstone/badwords.txt")) {
    download.file("http://badwordslist.googlecode.com/files/badwords.txt", 
                  "data/capstone/badwords.txt")
}

# Remove puntication
corpus <- tm_map(corpus, removePunctuation)


corpus <- tm_map(corpus, removePunctuation) 


undisered <- scan("data/capstone/badwords.txt","")
undisered <- gsub("[*]", "", undisered)
final2 <- tm_map(final1, removeWords, undisered)


tdm <- TermDocumentMatrix(final)
