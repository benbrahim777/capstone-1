library(shiny)
library(data.table)
library(NLP)
library(tm)

# Load model data
Freq <- readRDS("FreqShiny.obj")
Ref <- readRDS("RefShiny.obj")



n1 <- integer()
n2<- integer()
n3<-integer()
n4<-integer()
Y<- integer()
D1<-double()
D2<-double()
D3<-double()
for (i in 1:4) {
  n1[i] <- nrow(Freq[[i+1]][C==1])
  n2[i] <- nrow(Freq[[i+1]][C==2])
  n3[i] <- nrow(Freq[[i+1]][C==3])
  n4[i] <- nrow(Freq[[i+1]][C==4])
  Y[i]<- n1[i]/(n1[i]+(2*n2[i]))
  D1[i] <- 1-(2*Y[i]*(n2[i]/n1[i]))
  D2[i] <- 2-(3*Y[i]*(n3[i]/n2[i]))
  D3[i] <- 3-(4*Y[i]*(n4[i]/n3[i]))
  
}


kneserNey <- function(words){
  nwords <- length(words)
  #Kenieser-Ney algorithm
  # High order element
  # Return probability of all n-grams execpt where prediction is <UKN>
  # Delta caclculation
  Pkn2 <- Freq[[nwords+1]][words] # N+1 gram
  N1 <- Pkn2[C==1, .N]
  N2 <- Pkn2[C==2, .N]
  N3 <- Pkn2[C>=3, .N]
  delta <- 0.8
  PknRows <- nrow(Pkn2)
  Cw <- Freq[[nwords]][words]$C
  #Pkn2 <- Pkn2[,Pkn:=(C-delta)/Cw]
  Pkn2[C==1,Pkn:=(C-D1[nwords])/Cw]
  Pkn2[C==2,Pkn:=(C-D2[nwords])/Cw]
  Pkn2[C>3,Pkn:=(C-D3[nwords])/Cw]
  #lambda <- (delta/Cw)*PknRows
  lambda <- ((D1[nwords]*N1+D2[nwords]*N2+D3[nwords]*N3)/Cw)
  
  # If only one row return and no probability (word not found)
  # return lower portion of formula
  if (PknRows==1) { 
    if (is.na(Pkn2$Pkn) | Pkn2$n1 == 1) {
      lambda <- 1
      Pkn2$Pkn <- 0
    }
  }
  
  # Call algorithm recursevely
  if (nwords == 1) {
    # If lower order just return pcont
    Pkn1<- Freq[[nwords]][Pkn2[,.(n1)], .(n1, Pkn)]
  } else {
    # More than one word
    # No higher order n-gram
    if (PknRows==1) {
      if (Pkn2$Pkn == 0 | Pkn2$n1 == 1) {
        Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)]
      } else {
        Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)]
      } 
    } else {
      Pkn1 <- kneserNey(words[2:nwords])[,.(n1,Pkn)]
    }
  } 
  setkey(Pkn2, n1)
  setkey(Pkn1, n1)
  
  #Merge with Pkn1 so same dimensions and replace NAs with 0
  Pkn2 <- merge(Pkn2[,.(n1,Pkn)], Pkn1[,.(n1)], all.y=T)
  Pkn2[is.na(Pkn), Pkn:=0]
  Pkn2[, Pkn:=Pkn2$Pkn+lambda*Pkn1$Pkn][!(n1==1)]     
} 

Pkfun <- function(text) {
  #Format text
  text <- tolower(text)
 # text <- removeNumbers(text)
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
    #reference <- Ref[grep(paste0("^",palabra), n1)][n1!=palabra]$I
    reference <- Ref[grep(paste0("^",palabra), n1)]$I
    # If no word found use first letter
    if (length(reference) ==0) {
      #reference <- Ref[grep(paste0("^", substr(palabra,1,1)), n1)][n1!=palabra]$I
      #reference <- Ref[grep(paste0("^", substr(palabra,1,1)), n1)]$I
      reference <- 1
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



wordsCompleted <- 0
wordsPredicted <- 0
keyStrokes <- 0
previousWord <- ""
update <- FALSE

shinyServer(function(input, output, session) {
  words <- reactive({
      words <- Pkfun(input$textt)$n1[1:3]
      #browser()
      textsplit <- input$textt
      #if (textsplit =="" | is.null(input$mydata)) return(words)
      if (textsplit =="") return(words)
      textsplit <- unlist(strsplit(textsplit, " "))
      last <- length(textsplit)
      isolate({
        if (space() & update & input$mydata!=8){
          keyS <- 0
          wordsPredicted <<- wordsPredicted + 1
 #          browser()
          textsplit <- paste0(textsplit[-last], collapse = " ")
          #textsplit <- paste0(textsplit, " ")          
          wordsCompleted <<- wordsCompleted + 1
          keyStrokes <<- keyStrokes + nchar(words[1])- keyS
          updateTextInput(session,inputId = "textt",
                          value = paste0(textsplit, " ", previousWord," "))  
          session$sendCustomMessage(type="cursorEndd", "textt")
          update <<- FALSE
        } else if (input$mydata!=8 & !space()){
          update <<- TRUE
#          browser()
        }
      })
      previousWord <<- words[1]
      if (previousWord == "<UKN>") {
        #browser()
        previousWord <<- textsplit[last]
        words[1] <- previousWord
      }
      return(words)
  })

# output$results <- renderPrint({
#   paste(unlist(strsplit(input$textt, " "))[length(input$textt)],input$mydata)
# })

  space <- reactive({
    textsplit <- input$textt
    if (substr(textsplit,nchar(textsplit),nchar(textsplit)) == " " |
          textsplit=="") space <- TRUE
      else space <- FALSE
  })

#   mensaje <- reactive({
#     input$text
#   })
  
  # You can access the value of the widget with input$text, e.g.
  output$value1 <- renderUI({
    word <- words()[1]
    if(!is.na(word)) { 
      if (word=="<UKN>") {
        a <- unlist(strsplit(input$textt, " "))
        word <- a[length(a)]}
      actionButton("action1", label = word, style="color:blue;float:left;width:34%;")
   }
  })
  output$value2 <- renderUI({
    word <- words()[2]
    if(!is.na(word)) { 
      if (word=="<UKN>") {
        a <- unlist(strsplit(input$textt, " "))
        word <- a[length(a)]}
      actionButton("action2", label = word, style="color:grey;float:left;width:33%;")
      }
  })
  output$value3 <- renderUI({
    word <- words()[3]
    if(!is.na(word)) { 
      if (word=="<UKN>") {
        a <- unlist(strsplit(input$textt, " "))
        word <- a[length(a)]}
      actionButton("action3", label = word, style="color:grey;float:right;width:33%;")
    }
  })

  output$wordsCompleted <-renderText({
    input$action1
    input$action2
    input$action3
    input$textt
    paste0("Words Completed: ",wordsCompleted)
  })
  output$wordsPredicted <-renderText({
    input$action1
    input$action2
    input$action3
    input$textt
    paste0("Words Predicted: ",wordsPredicted)
  })
  output$keyStrokes <-renderText({
    input$action1
    input$action2
    input$action3
    input$textt
    paste0("Key Strokes Saved: ", keyStrokes)
  })
  
  
  observe({
    if(!is.null(input$action1)) {
      if(input$action1==0) return(NULL)
    #input$x
    
    update <<-FALSE
    isolate({
      textsplit <- input$textt
      keyS <- 0
      if (!space()) {
        textsplit <- unlist(strsplit(textsplit, " "))
        last <- length(textsplit)
        keyS <- nchar(textsplit[last])
        textsplit <- paste0(textsplit[-last], collapse = " ")
        textsplit <- paste0(textsplit, " ")
        wordsCompleted <<- wordsCompleted + 1
      } else {
        wordsPredicted <<- wordsPredicted + 1
      }
      keyStrokes <<- keyStrokes + nchar(words()[1])- keyS
      updateTextInput(session,inputId = "textt",
                      value = paste0(textsplit, words()[1]," "))  
      session$sendCustomMessage(type="cursorEndd", "textt")
    })
    }
    
  })
  
  observe({
    if(!is.null(input$action2)) {
      if(input$action2==0) return(NULL)
    #text <- input$text
    
    update <<-FALSE
    isolate({
      textsplit <- input$textt
      keyStrokes <-0
      if (!space()) {
        textsplit <- unlist(strsplit(textsplit, " "))
        textsplit <- paste0(textsplit[-length(textsplit)], collapse=" ")
        textsplit <- paste0(textsplit, " ")
        wordsCompleted <<- wordsCompleted + 1
      } else {
        wordsPredicted <<- wordsPredicted + 1
      }
      updateTextInput(session,inputId = "textt",
                      value = paste0(textsplit, words()[2], " "))  
      session$sendCustomMessage(type="cursorEndd", "textt")
    })
    }
  })
  
  observe({
    if(!is.null(input$action3)) {
      if(input$action3==0) return(NULL)
    #text <- input$text
    
    update <<-FALSE
    isolate({
      textsplit <- input$textt
      keyStrokes <-0
      if (!space()) {
        textsplit <- unlist(strsplit(textsplit, " "))
        textsplit <- paste0(textsplit[-length(textsplit)], collapse=" ")
        textsplit <- paste0(textsplit, " ")
        wordsCompleted <<- wordsCompleted + 1
      } else {
        wordsPredicted <<- wordsPredicted + 1
      }
      updateTextInput(session = session,inputId = "textt",
                    value = paste0(textsplit, words()[3], " "))
      session$sendCustomMessage(type="cursorEndd", "textt")
    })
    } 
  })
  
})

