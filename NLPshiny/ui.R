library(shiny)

shinyUI(fluidPage(
  titlePanel("Next Word Predictor"),
  fluidRow(
      column(3,
            wellPanel( 
              h4("Instructions"),
              p(paste0(
                "Words are predicted as you type. At most 3 options are provided. ",
                "Type <SPACE> to select the top predicted word (in blue in the center) or press",
                " any of the three words to select the predicted word you like. A space is needed",
                " after the last word for the next word to be predicted")))),
      column(4,
             #textInput("textt", label="Enter text here:"),
             h4("Enter text here:"),
             tags$textarea(id="textt",style="width:100%; resize: vertical;", rows=4),
             singleton(tags$head(tags$script(src = "newevent.js"))),
             singleton(tags$head(tags$script('
                       $(document).on("keydown", function (e) {
                       Shiny.onInputChange("mydata", e.which);
                       });
                       '))),
             fluidRow(
             uiOutput("value1"), 
             uiOutput("value2"), 
             uiOutput("value3"))),
      column(3, 
            wellPanel(
            h4("Statistics"),
            textOutput("wordsCompleted"),
            textOutput("wordsPredicted"),
            textOutput("keyStrokes")
            #verbatimTextOutput("results")
      )))
    
    
   

    
    
  )
)
