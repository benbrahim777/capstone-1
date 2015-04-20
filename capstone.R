# Some words are just start or end words, so no common enough to consider

library(data.table)
library(tm)
#library(ngram)
library(SnowballC)
library(parallel)
library(htmlwidgets)
library(caret)
if (!require("DT")) devtools::install_github("rstudio/DT")
library(DT)
#library(rJava) 
#.jinit(parameters="-Xmx128g")
#library(RWeka)
library(ggplot2)
#library(devtools)
#if(!require("tmt")) install("../R/x86_64-pc-linux-gnu-library/3.1/tmt1")
#library(tmt)

# Cluster initialization
cluster <- makeCluster(detectCores())
clusterEvalQ(cluster, library(data.table))
clusterEvalQ(cluster, library(tm))
#clusterEvalQ(cluster, library(ngram))
#clusterEvalQ(cluster, library(tmt))
options(mc.cores=4)

fileNames <- list.files("data/capstone/final/en_US", pattern="*.txt")
cons <- lapply(paste0("data/capstone/final/en_US/",fileNames), file, open = "r", encoding = "UTF-8")
files <- lapply(cons, readLines, skipNul=TRUE) # NUL characters to skip
close(cons[[1]])
close(cons[[2]])
close(cons[[3]])

if (!file.exists("data/capstone/badwords.txt")) {
  download.file("http://badwordslist.googlecode.com/files/badwords.txt", 
                "data/capstone/badwords.txt")
}

sets <- c(files[[1]], files[[2]], files[[3]])

rm(files)

set.seed(1023)
firstSplit <- createDataPartition(1:length(sets), p=.2, list=FALSE)
sets <- sets[firstSplit]
trainingIndices <- createDataPartition(1:length(sets), p=.8, list=FALSE)
trainingSet <- sets[trainingIndices]
sets <- sets[-trainingIndices]
testIndices <- createDataPartition(1:length(sets), p=.5, list=FALSE)
testSet <- sets[testIndices]
validationSet <- sets[-testIndices]

sets <- list(trainingSet, testSet, validationSet)
rm(trainingSet, testSet, validationSet)

# Required by tmt
#Sys.setlocale('LC_ALL', 'C')

#sets <- lapply(aspellStem, clean=T, stops="")
`# Remove non-ASCII characters
sets <- lapply(sets, iconv, "UTF-8", "ASCII", sub="")

# Replace hyphen with spaces
sets <- lapply(sets, function(x) gsub("-", " ", x))

# Remove non-alphanumeric
sets <- lapply(sets, function(x) gsub("[^[:alnum:][:space:]]", "", x))
# remove spaces

#sets <- lapply(sets, function(x) gsub('[^a-zA-Z0-9 ]', '', x))

# Remove words with characters repeated more than 2 times and sub with one char 
# (most common occurrence)
sets <- lapply(sets, function(x) gsub("([[:alpha:]])\\1{2,}", "\\1", x))

#To lower
sets <- lapply(sets, tolower)

# Project only requires to cover US English langauge
corpus <- Corpus(VectorSource(sets))
rm(sets)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)

undisered <- readLines("data/capstone/badwords.txt")
undisered <- removePunctuation(undisered)
undisered <- removeNumbers(undisered)
#corpus <- tm_map(corpus, removeWords, undisered)


#corpus[[1]]$content <- corpus[[1]]$content[1:200000]
#Unigrams

Freq <- list()
Freq[[1]] <- data.table(n1=unlist(strsplit(corpus[[1]]$content[1:200000]," ")))
Perplexity <- data.table(n1=unlist(strsplit(corpus[[2]]$content," ")))

# Preserve oreder to create n-grams
Freq[[1]][,I:=.I]
# Count occurrences
Freq[[1]] <- Freq[[1]][,.(C=.N, I), by=n1]
setkey(Freq[[1]], n1)

# Only those that occur more than 4 or less than 15 chars long and not empty

Freq[[1]][(C<=5) | (nchar(n1) >15) | (n1 ==""), n1:="<UKN>"]

# Remove undisered
Freq[[1]][n1 %in% undisered, n1:="<UKN>"]

# Create reference for words so we can reference by number
Ref <- Freq[[1]][,.(C=.N), by=n1]
setkey(Ref, n1)

# Syntax correct too slow. skipping
# sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
# correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }
# Ref[, n1C:=correct(n1), by=n1]
# Ref[, C:=sum(C), by=n1C]

# Stemmed version
Ref <- Ref[,.(n1, n1S=stemDocument(n1))]
Ref <- Ref[, .(n1,.I, IS=min(.I)), by=list(n1S)]

setkey(Freq[[1]],n1)
Freq[[1]] <- merge(Freq[[1]], Ref[,.(n1,n1n=I, n1S=IS)])
Freq[[1]] <- Freq[[1]][,.(n1=n1n, n1S, I)]
Freq[[1]] <- Freq[[1]][order(I)]

#Bigrams
# This will create grams between sentences, but might not be a bad idea
Freq[[2]] <- Freq[[1]][, .(n2=n1S, n1=n1[.I+1])]
Freq[[2]] <- Freq[[2]][1:(nrow(Freq[[2]])-1)]
setkey(Freq[[2]],n2,n1)
Freq[[2]] <- Freq[[2]][,.(C=.N), by=list(n2,n1)]

#trigrams
Freq[[3]] <- Freq[[1]][, .(n3=n1S, n2=n1S[.I+1], n1=n1[.I+2])]
Freq[[3]] <- Freq[[3]][1:(nrow(Freq[[3]])-2)]
setkey(Freq[[3]],n3,n2,n1)
Freq[[3]] <- Freq[[3]][,.(C=.N), by=list(n3,n2,n1)]

#quadgrams
Freq[[4]] <- Freq[[1]][, .(n4=n1S, n3=n1S[.I+1], n2=n1S[.I+2], n1=n1[.I+3])]
Freq[[4]] <- Freq[[4]][1:(nrow(Freq[[4]])-3)]
setkey(Freq[[4]],n4,n3,n2,n1)
Freq[[4]] <- Freq[[4]][,.(C=.N), by=list(n4,n3,n2,n1)]

#pentagrams (No memory)
Freq[[5]] <- Freq[[1]][, .(n5=n1S, n4=n1S[.I+1], n3=n1S[.I+2], n2=n1S[.I+3], n1=n1[.I+4])]
Freq[[5]] <- Freq[[5]][1:(nrow(Freq[[5]])-4)]
setkey(Freq[[5]],n5, n4,n3,n2,n1)
Freq[[5]] <- Freq[[5]][,.(C=.N), by=list(n5,n4,n3,n2,n1)]

Freq[[1]] <- Freq[[1]][,.(n1S=unique(n1S), C=.N), by=n1]



saveRDS(corpus, "data/capstone/corpus.obj")
corpus <- readRDS("data/capstone/corpus.obj")
rm(corpus)


# Simple Good-Turing analysis
#http://www.d.umn.edu/~tpederse/Courses/CS8761-FALL02/Code/sgt-gale.pdf
nrzest<-function(r, nr)
{
    d <- c(1, diff(r))
    dr <- c(0.5 * (d[-1] + d[ - length(d)]), d[length(d)])
    return(nr/dr)
}

# Used by Simple Good-Turing analysis
rstest<-function(r, coef)
{
  return(r * (1 + 1/r)^(1 + coef[2]))
}

# Takes count type and frequecies
gt <- function ( xr, xnr) {
  xN<-sum(xr*xnr)
  
  #make averaging transform
  xnrz<-nrzest(xr,xnr)
  
  #get Linear Good-Turing estimate
  xf<-lsfit(log(xr),log(xnrz))
  xcoef<-xf$coef
  xrst<-rstest(xr,xcoef)
  xrstrel<-xrst/xr
  
  #get Turing estimate
  xrtry<-xr==c(xr[-1]-1,0)
  xrstarel<-rep(0,length(xr))
  xrstarel[xrtry]<-(xr[xrtry]+1)/xr[xrtry]*c(xnr[-1],0)[xrtry]/xnr[xrtry]
  
  #make switch from Turing to LGT estimates
  tursd<-rep(1,length(xr))
  for(i in 1:length(xr)) if(xrtry[i])
    tursd[i]<-(i+1)/xnr[i]*sqrt(xnr[i+1]*(1+xnr[i+1]/xnr[i]))
  xrstcmbrel<-rep(0,length(xr))
  useturing<-TRUE
  for(r in 1:length(xr)){
    if(!useturing) xrstcmbrel[r]<-xrstrel[r]
    else if(abs(xrstrel-xrstarel)[r]*r/tursd[r] > 1.65)
      xrstcmbrel[r]<-xrstarel[r]
    else {useturing<-FALSE; xrstcmbrel[r]<-xrstrel[r]}
  }
  
  #renormalize the probabilities for observed objects
  sumpraw<-sum(xrstcmbrel*xr*xnr/xN)
  xrstcmbrel<-xrstcmbrel*(1-xnr[1]/xN)/sumpraw
  return(xrstcmbrel)
}  


# Sorted array of frequencies of counts
CNr <- Freq[[1]][order(C), .(Nr=.N), by=C]
V <- sum(CNr$C*CNr$Nr)
CNr[, Pr:=gt(C,Nr)/V]
CNr[, Zr:=nrzest(C,Nr)]
CNr[, Cr:=((C+1)*CNr[.I+1, .(Zr)])/Zr]

# Probability of unseen
CNr[C==1,.(Nr/V)]

#CNr[,Zr1:=(Nr/(0.5*(CNr[.I+1,C]-c(NA, C[seq_len(.N-1)]))))][,logZr:=log(Zr)]

#par(mfrow=c(1,2))
plot(CNr[,.(log(C),log(Zr))])


#CNr[,Var:=((C+1)^2)*(CNr[.I+1,Nr]/Nr^2)*((1+CNr[.I+1,Nr])/Nr)]
# Calculate probabilities for each gram
setkey(Freq[[1]], C)
setkey(CNr, C)
Freq[[1]] <- Freq[[1]][CNr[,.(C, Pr)]]
#unigramFreq[,P:=SGT/V]

# Sorted array of frequencies of counts
CNr <- Freq[[2]][order(C), .(Nr=.N), by=C]
V <- sum(CNr$C*CNr$Nr)
CNr[, Pr:=gt(C,Nr)/V]
CNr[, Zr:=nrzest(C,Nr)]
CNr[, Cr:=((C+1)*CNr[.I+1, .(Zr)])/Zr]


# Probability of unseen
CNr[C==1,.(Nr)]/V

# Add one smoothing first
#bigramFreq[, P:=bigramFreq[,.(C)]/unigramFreq[bigramFreq$n1, .(C)]
setkey(Freq[[2]], C)
setkey(CNr, C)
Freq[[2]] <- Freq[[2]][CNr[,.(C, Pr)]]


# Sorted array of frequencies of counts
CNr <- Freq[[3]][order(C), .(Nr=.N), by=C]
V <- sum(CNr$C*CNr$Nr)
CNr[, Pr:=gt(C,Nr)/V]
CNr[, Zr:=nrzest(C,Nr)]
CNr[, Cr:=((C+1)*CNr[.I+1, .(Zr)])/Zr]

# Probability of unseen
CNr[C==1,.(Nr)]/V

# Add one smoothing first
#bigramFreq[, P:=bigramFreq[,.(C)]/unigramFreq[bigramFreq$n1, .(C)]
setkey(Freq[[3]], C)
setkey(CNr, C)
Freq[[3]] <- Freq[[3]][CNr[,.(C, Pr)]]

# Sorted array of frequencies of counts
CNr <- Freq[[4]][order(C), .(Nr=.N), by=C]
V <- sum(CNr$C*CNr$Nr)
CNr[, Pr:=gt(C,Nr)/V]
CNr[, Zr:=nrzest(C,Nr)]
CNr[, Cr:=((C+1)*CNr[.I+1, .(Zr)])/Zr]

# Probability of unseen
CNr[C==1,.(Nr)]/V

# Add one smoothing first
#bigramFreq[, P:=bigramFreq[,.(C)]/unigramFreq[bigramFreq$n1, .(C)]
setkey(Freq[[4]], C)
setkey(CNr, C)
Freq[[4]] <- Freq[[4]][CNr[,.(C, Pr)]]

# Sorted array of frequencies of counts
CNr <- Freq[[5]][order(C), .(Nr=.N), by=C]
V <- sum(CNr$C*CNr$Nr)
CNr[, Pr:=gt(C,Nr)/V]
CNr[, Zr:=nrzest(C,Nr)]
CNr[, Cr:=((C+1)*CNr[.I+1, .(Zr)])/Zr]

# Probability of unseen
CNr[C==1,.(Nr)]/V

# Add one smoothing first
#bigramFreq[, P:=bigramFreq[,.(C)]/unigramFreq[bigramFreq$n1, .(C)]
setkey(Freq[[5]], C)
setkey(CNr, C)
Freq[[5]] <- Freq[[5]][CNr[,.(C, Pr)]]

setkey(Freq[[1]], n1)
setkey(Freq[[2]], n2, n1, Pr)
setkey(Freq[[3]], n3, n2, n1, Pr)
setkey(Freq[[4]], n4, n3, n2, n1, Pr)
setkey(Freq[[5]], n5, n4, n3, n2, n1, Pr)


# Continuation probability
# What to do with words that do not have a preciding word
# d is approximatelly 0.53
delta <- 0.75
#a <- Freq[[1]][Freq[[2]][,.(n2,n1,.N), by=n2]][,.(n1,N)]
#Freq[[1]] <- merge(a, Freq[[1]], all.y=T)
a <- Freq[[2]][,.N, by=n1]
setkey(a, n1)
Freq[[1]] <- merge(a, Freq[[1]])
Types <- nrow(Freq[[2]])
Freq[[1]][,Pkn:=N/Types]

setkey(Ref, n1, n1S, I)
# Probability

saveRDS(Freq, "data/capstone/Freq.obj")
saveRDS(Ref, "data/capstone/Ref.obj")
Freq <- readRDS("../data/capstone/Freq.obj")

#Objects for Shinny app
Freq[[1]] <- Freq[[1]][,.(n1, C, Pkn)]
setnames(Freq[[2]], "Pr", "Pkn")
setnames(Freq[[3]], "Pr", "Pkn")
setnames(Freq[[4]], "Pr", "Pkn")
setnames(Freq[[5]], "Pr", "Pkn")
Ref <- Ref[, .(n1, I, IS)]
saveRDS(Freq, "NLPshinny/FreqShiny.obj")
saveRDS(Ref, "NLPshinny/RefShiny.obj")

kneserNey <- function(words){
  nwords <- length(words)
  #Kenieser-Ney algorithm
  # High order element
  # Return probability of all n-grams execpt where prediction is <UKN>
  Pkn <- Freq[[nwords+1]][words][n1!=1] # N+1 gram
  PknRows <- nrow(Pkn)
  Cw <- Freq[[nwords]][words]$C
  Pkn <- Pkn[,Pkn:=(C-delta)/Cw]
  lambda <- (delta/Cw)*PknRows
  
  # If only one row return and no probability (word not found)
  # return lower portion of formula
  if (PknRows==1) { 
    if (is.na(Pkn$Pkn)) {
      lambda <- 1
      Pkn$Pkn <- 0
    }
  }
  
  # Call algorithm recursevely
  if (nwords == 1) {
    # If lower order just return pcont
    Pkn1<- Freq[[nwords]][Pkn[,.(n1)], .(n1, Pkn)][is.na(Pkn), Pkn:=0]
    #Pkn1<- Freq[[nwords]][words, .(n1, Pkn=pcont)]
  } else {
    # More than one word
    # No higher order n-gram
    if (PknRows==1) {
      if (Pkn$Pkn == 0) {
        Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)]
        #(gsub("^\\w+ ","", text))[,.(n1=Ref[n1]$I,Pkn)]
        #Pkn <- Pkn1
      } else {
        Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)]
        #Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)][n1==Pkn$n1]
        #Pkn1 <- kneiserNey(gsub("^\\w+ ","", text))[,.(n1=Ref[n1]$I,Pkn)][n1==Pkn$n1]
        #Pkn1 <- Pkfun(gsub("^\\w+ ","", text))[,.(n1=Ref[n1]$I,Pkn)]
      } 
    } else {
      Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)]
      #Pkn1 <- kneserNey(words[2:nwords])[n1==Pkn$n1]
      #Pkn1 <- Pkfun(gsub("^\\w+ ","", text))[,.(n1=Ref[n1]$I,Pkn)]
    }
  } 
  setkey(Pkn, n1)
  setkey(Pkn1, n1)
  
  #Merge with Pkn1 so same dimensions and replace NAs with 0
  Pkn <- merge(Pkn[,.(n1,Pkn)], Pkn1[,.(n1)], all.y=T)
  Pkn[is.na(Pkn), Pkn:=0]
  Pkn[, Pkn:=Pkn+lambda*Pkn1$Pkn]          
} 



Pkfun <- function(text) {
  #Format text
  text <- tolower(text)
  text <- removeNumbers(text)
  text <- removePunctuation(text)
  text <- stripWhitespace(text)
  
  # Split text into word vector each index
  words <- unlist(strsplit( text,split = " "))
  
  # When there is a space at the end we can assume that the user selected the previous
  # word and it expects a prediction for the next
  space <- FALSE
  if (substr(text,nchar(text),nchar(text)) == " ") space <- TRUE
  
  # Number of words
  nwords <- length(words)

  #When empty return the unigram based on Pcontinuation
  if (nwords==0) return(Freq[[1]][order(Pkn, decreasing=T, 
                              na.last=NA)][n1!=1][,.(n1=Ref[n1]$n1, Pkn)])
  
  # We can only predcit with the penta-gram
  if (nwords > 4) {
    words<- words[(nwords-3):nwords]
    nwords <- 4
    text1 <- paste(words, collapse = " ")
    if(space) {text <- paste0(text1, " ")}
  }
  
  #FUTURE If word does not exist, try to split
  
  # Save last word fo the furure
  palabra <- words[nwords]
  
  # Assign reference to stemmed index
  wordsIndex <- integer()
  for (i in 1:nwords) {
    # Index of stem version of the word
    # if any of the words dos not exist, replace with <UKN> reference
    if (nrow(Ref[n1==words[i]])==0) { wordsIndex[i] <- 1
    } else { wordsIndex[i] <- Ref[n1==words[i]]$IS }
  }
  
    # Convert to list for data table referencing
  words <- as.list(wordsIndex)
  
  # If the last word is not complete we will predict based on the characters the user has typed
  if (!space) {
    # Reference to all words that start with the text typed so far
    reference <- Ref[grep(paste0("^",palabra), n1)][n1!=palabra]$I
    # If no word found use first letter
    if (length(reference) ==0) {
      reference <- Ref[grep(paste0("^", substr(palabra,1,1)), n1)][n1!=palabra]$I
    }    
    # If only one word use pcontinuation
    if( nwords==1) {
      Pkn <- Freq[[nwords]][n1==reference,.(n1,Pkn)][!is.na(Pkn)]
    } else {
      for (i in 1:nwords) {
          Pkn <- Freq[[nwords-i+1]][words[i:(nwords-1)]][n1==reference,.(n1, Pkn)]
          if(nrow(Pkn)!=0) break
          Pkn <- Freq[[1]][n1==reference,.(n1,Pkn)][!is.na(Pkn)]
      }
    }
  # This is when the word is complete and we need to predict next word  
  } else {
      # Call kneiser-ney
      Pkn <- kneserNey(words)
  }    

  return(Pkn[,.(n1=Ref[n1]$n1,Pkn)][order(Pkn, decreasing=T)])
  
}
#   if(nwords==1 & substr(text,nchar(text),nchar(text)) != " ") {
#   }
# 
#   if(substr(text,nchar(text),nchar(text)) != " ") {
#     Pk <- unigramFreq[grep(paste0("^",words[1]), n0)][order(pcont, decreasing = T)][1:3]
#   } else {
#     Pk1 <- unigramFreq[word[1]]$pcont
#     Cw <- unigramFreq[words[1]]$C
#     Pk2 <- bigramFreq[words[1], .(n1, n0, Pk2=(C-delta)/Cw)]
#     lambda <- (delta/Cw)*length(Pk2)
#     Pk <- unigramFreq[words[1],.(n0=pred$n0, Pk2=pred$Pk2+lambda*Pk1)]
#   }

#  return(Pk)
#}

# Back-off
# PnextWord <-  function(text) {
#     words <- unlist(strsplit( text,split = " "))
#     words <- removePunctuation(words)
#     words <- tolower(words)
#     
#     space <- 0
#     if(substr(text,nchar(text),nchar(text)) == " ") {
#         space <-0.5
#         print("what")}
#     S <- paste0("C", length(words) + space)
#     print(S)
#     
#     switch (S,
#             C1={
#                 unigramFreq[grep(paste0("^",words[1]), n0)][order(pcont, decreasing = T)][1:3]
#             },
#             C1.5={
#                 Cw1 <- unigramFreq[words[1]]$C
#                 pred2 <- bigramFreq[words[1], .(n1, n0, Pr=(C-delta)/Cw1)]
#                 lambda1 <- (delta/Cw1)*length(pred2)
#                 pred1 <- unigramFreq[words[1],.(n0=pred2$n0, Pr=pred2$Pr+lambda1*pcont)]
#                 pred1[order(Pr, decreasing = T)][1:3]
#             },
#             C2={
#               Cw1 <- unigramFreq[words[1]]$C
#               pred2 <- bigramFreq[words[1]][grep(paste0("^",words[2]), n0)][, .(n1, n0, Pr=(C-delta)/Cw1)]
#               lambda1 <- (delta/Cw1)*length(pred2)
#               pred1 <- unigramFreq[words[1],.(n0=pred2$n0, Pr=pred2$Pr+lambda1*pcont)]
#               pred1[order(Pr, decreasing = T)][1:3]
#             },
#             C2.5={
#               Cw2 <- trigramFreq[words[1]][n1==words[2]]$C
#               pred3 <- trigramFreq[words[1]][n1==words[2], .(n2, n1, n0, Pr=(C-delta)/Cw2)]
#               lambda2 <- (delta/Cw2)*length(pred3)
#               Cw1 <- unigramFreq[words[1]]$C
#               pred2 <- bigramFreq[words[1], .(n1, n0, Pr=(C-delta)/Cw1)]
#               lambda1 <- (delta/Cw1)*length(pred2)
#               pred1 <- unigramFreq[words[1],.(n0=pred3$n0, Pr=pred3$Pr+lambda2*(pred2$Pr+lambda1*pcont))]
#               pred1[order(Pr, decreasing = T)][1:3]
#               
#               
# #               pred2 <- bigramFreq[words[2]][n0==pred3$n0]
# #                 pred1 <- unigramFreq[pred2$n0]
# #                 pred1[, .SD[which.max(0.6*pred3$P + 0.3*pred2$P + 0.1*P)]]
#                 #trigramFreq[words[1]][n1==words[2],.SD[which.max(P)]]              
#             },
#             C3={
#                 trigramFreq[words[1]][n1==words[2]][grep(paste0("^",words[3]), n0), .SD[which.max(P)]]              
#             },
#       stop()
# )
# }
  



