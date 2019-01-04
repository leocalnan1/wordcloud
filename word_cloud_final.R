#Libraries required for functionality of code
#install.packages is commented so they do not load when the app is started

#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("shiny")
#install.packages("shinythemes")
#install.packages("shinyWidgets")
#install.packages("memoise")
library(memoise)
library(shinyWidgets)
library(shinythemes)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(shiny)

#Raw Data
setwd("C:/Users/Admin/Documents/")
WordsAndFrequencies <- read.csv("WordsAndFrequencies.csv", header = TRUE, stringsAsFactors = FALSE)
hotel_reviews_v2 <- read.csv("hotel_reviews_v2.csv", header = TRUE, stringsAsFactors = FALSE)

#Gets reviews for each hotel
hotel_reviews_name <- function(name){
  hotel_name <- subset(hotel_reviews_v2, hotel.name == name)
  hotel_name_reviews <- hotel_name$reviews.text
  return(hotel_name_reviews)
}

#Gets reviews for each rating
hotel_reviews_rating <- function(rating){
  hotel_rating <- subset(hotel_reviews_v2,  reviews.rating == rating)
  hotel_rating_reviews <- hotel_rating$reviews.text
  return(hotel_rating_reviews)
}

#cleans text for usable English words /ie/ no German, Russian, etc.
corpusify <- memoise(function(text_file){
  docs <- Corpus(VectorSource(text_file))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("2","der","die","das","sehr","auch","war","mit","wir","und","sie","I","the","The","and","a","as","an"))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  
  return(docs)
})

### Front End ###
ui <- fluidPage(
  #set background colour
  setBackgroundColor("White"),
  #set theme of app
  theme <- shinytheme("sandstone"),
  #colour of horizontal line to space the sidebar panel
  tags$head(
    tags$style(HTML("hr {border-top: 2px solid #a9a9a9;}"))),
  #title of app
  titlePanel("The XQAC Hotel Word Cloud"),
  #sidebar
  sidebarPanel(
    #drop down menu for hotel name
    selectInput("hotel", "Choose Hotel",
                choices = hotel_reviews_v2$hotel.name,
                selected = ""
    ),
    #horizontal line
    hr(),
    #Buttons to choose the rating
    radioButtons("rating", label = "Select Rating (Stars). Select a rating to show the Wordcloud:",
                 choices = list("1" = 1,
                                "2" = 2,
                                "3" = 3,
                                "4" = 4,
                                "5" = 5),
                 selected = ""
    ),
    #horizontal line
    hr(),
    #Slider for the frequency of words in the text
    sliderInput(inputId = "freq",
                label = "Frequency of Words:",
                value = 15, min = 0, max = 30),
    #Slider for the number of words in the text
    sliderInput(inputId = "max",
                label = "Number of Words:",
                value = 50, min = 1, max = 100)),
  #Main wordcloud panel
  mainPanel(
    tabsetPanel(
      #tab 1
      tabPanel("Sort by Hotel Name", plotOutput("plot1", width = "100%", height = "600px")),
      #tab 2
      #Shows an error, just select a button
      tabPanel("Sort by Hotel Rating", plotOutput("plot2", width = "100%", height = "600px")),
      #tab 3
      tabPanel("All Data", plotOutput("plot3", width = "100%", height = "600px"))
    )
  )
)

### Back End ###
server <- function(input,output,session){
  #Wordcloud for different hotels
  output$plot1 <- renderPlot({
    set.seed(183565743)
    wordcloud(corpusify(hotel_reviews_name(input$hotel)),
              scale = c(4, 2),
              min.freq = input$freq,
              max.words= input$max, 
              random.order=FALSE, 
              rot.per=0.318, 
              colors=brewer.pal(n=8, name = "Dark2"))
  })
  #Wordcloud for different ratings
  output$plot2 <- renderPlot({
    set.seed(183565743)
    wordcloud(corpusify(hotel_reviews_rating(input$rating)),
              scale = c(4, 2),
              min.freq = input$freq,
              max.words= input$max, 
              random.order=FALSE, 
              rot.per=0.318, 
              colors=brewer.pal(n=8, name = "Dark2"))
  })
  #Wordcloud for both
  output$plot3 <- renderPlot({
    set.seed(183565743)
    wordcloud(words = WordsAndFrequencies$word,
              freq = WordsAndFrequencies$frequency,
              scale = c(4, 2),
              min.freq = input$freq,
              max.words=input$max, 
              random.order=FALSE, 
              rot.per=0.318, 
              colors=brewer.pal(n=8, name = "Dark2"))
  })
}

### Connection to shinyApp ###
shinyApp(ui = ui, server = server)




