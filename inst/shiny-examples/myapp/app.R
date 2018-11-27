#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(igraph)
library(tm)
library(SnowballC)
library(dplyr)
library(wordcloud2)

ui <- fluidPage(
  h6(" Text Analyzer Authored by:", tags$img(src ="K.JPG", height=100, width=100)),
  verbatimTextOutput("preface"),
  tags$img(src = "T.png"),
  br(),
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
  textAreaInput("model", "Place your text", value = " Domestication theory is an approach in science and technology studies and media studies that describe the processes by which innovations, especially new technology is 'tamed' or appropriated by its users. First, technologies are integrated into everyday life and adapted to daily practices. Secondly, the user and its environment change and adapt accordingly. Thirdly, these adaptations feedback into innovation processes in industry, shaping the next generation of technologies and services.",  
                width = 400, height = 200,cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
  
  textAreaInput("keywordlist", "Place your keywords", value = " technology, art",  
                width = 400, height = 200,cols = NULL, rows = NULL, placeholder = NULL, resize = NULL),
  
  selectInput("Plottype", label = "Select the Plot for Concepts:",
           choices = c("Network","DendPlot"), selected = "Network"),
  selectInput("Shape", label = "Select the Shape for Wordcloud:",
              choices = c("cardiod","diamond","star"), selected = "star"),
  sliderInput("NC", label = "Choose number of Words",
             min = 1, max = 30, value = 10, step = 1),
 sliderInput("NC1", label = "Choose number of Concepts for DendPlot",
             min = 1, max = 10, value = 2, step = 1),
  selectInput("cl", label = "Select Color for Word Cloud/Node Network",
             choices = c("red","light blue","green","pink","magenta","violet"), selected = "green"),
  sliderInput("Rotate", label = "Control for Size of WordCloud/ Node",
             min = 0.1, max = 1, value = 0.2, step = 0.1)
    ),
 mainPanel(
  
   wordcloud2Output("plot3"),
  plotOutput("plot1"),
   verbatimTextOutput("val2")
 )

)
)
server <- function(input,output)
{
  
 
  
  output$val2 <- renderPrint({
    txt = input$model
    docs <- Corpus(VectorSource(txt))
    docs <- tm_map(docs,removePunctuation)
    docs <- tm_map(docs, removeNumbers)   
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs,stemDocument)
    docs <- tm_map(docs,PlainTextDocument)
    dtm <- DocumentTermMatrix(docs)   
    freq =  colSums(as.matrix(dtm))
    wordcount2 =  data.frame( FREQUENCY= sort(freq,decreasing = TRUE))
    wordcount2 = head(wordcount2,input$NC)
    wordcount2$wordnames =  row.names(wordcount2)
    row.names(wordcount2)= 1 : NROW(wordcount2)
    
    txt2 = input$keywordlist
    docs2 <- Corpus(VectorSource(txt2))
    docs2 <- tm_map(docs2,removePunctuation)
    docs2 <- tm_map(docs2, removeNumbers)   
    docs2 <- tm_map(docs2, removeWords, stopwords("english"))
    docs2 <- tm_map(docs2, stripWhitespace)
    docs2 <- tm_map(docs2,stemDocument)
    docs2 <- tm_map(docs2,PlainTextDocument)
    dtm2 <- DocumentTermMatrix(docs2)   
    freq2 =  colSums(as.matrix(dtm2))
    keywordcount =  data.frame( FREQUENCY= sort(freq2,decreasing = TRUE))
    keywordcount$wordnames =  row.names(keywordcount)
    row.names(keywordcount)= 1 : NROW(keywordcount)
    
     cat(sprintf("\nMatching the key words with the content of the text we understand that the topics discussed here in the text are as follows\n\n"))
     cat(sprintf("%s",intersect(keywordcount$wordnames,wordcount2$wordnames)))
    
    
  })
  
  output$plot1 <- renderPlot({
    txt = input$model
     
    docs <- Corpus(VectorSource(txt))
    docs <- tm_map(docs,removePunctuation)
    docs <- tm_map(docs, removeNumbers)   
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs,stemDocument)
    docs <- tm_map(docs,PlainTextDocument)
    dtm <- DocumentTermMatrix(docs)   
    freq =  colSums(as.matrix(dtm))
    wordcount =  data.frame( FREQUENCY= sort(freq,decreasing = TRUE))
    wordcount2 = head(wordcount,input$NC)
  
    if(input$Plottype == "Network")
    {
    m  =  as.matrix(wordcount2)
    termMatrix <- m %*% t(m)
    g <- graph.adjacency(termMatrix,mode = "undirected",weighted = TRUE)
    g = simplify(g)
    V(g)$label <- V(g)$name
    V(g)$degree <- degree(g)
    set.seed(1)
    layout1 <- layout.fruchterman.reingold(g)
   
     plot(g, layout=layout1,vertex.size = 100*input$Rotate,vertex.color= input$cl, edge.arrow.size=0.5)
    }
    if(input$Plottype == "DendPlot")
    {
      dd <- dist(scale(wordcount2), method = "euclidean")
     fit = hclust(dd, method="ward.D") 
     plot(fit,main = "Cluster of Words",xlab = "words",ylab = "distance") # display dendogram
     groups = cutree(fit, k=input$NC1) # cut tree into 4 clusters
     rect.hclust(fit, k=input$NC1, border= input$cl)
     
    }
    
    
  })
  
  
  output$plot3 <- renderWordcloud2({
    
    docs <- Corpus(VectorSource(input$model))
    docs <- tm_map(docs,removePunctuation)
    docs <- tm_map(docs, removeNumbers)   
    docs <- tm_map(docs, removeWords, stopwords("english"))
    docs <- tm_map(docs, stripWhitespace)
    docs <- tm_map(docs,stemDocument)
    docs <- tm_map(docs,PlainTextDocument)
    dtm <- DocumentTermMatrix(docs)   
    freq =  colSums(as.matrix(dtm))
    wordcount2 =  data.frame( FREQUENCY= sort(freq,decreasing = TRUE))
    wordcount2 = head(wordcount2,input$NC)
    wordcount2$wordnames =  row.names(wordcount2)
    row.names(wordcount2)= 1 : NROW(wordcount2)
    wordcount2$freqcount =  wordcount2$FREQUENCY
    wordcount2$FREQUENCY = NULL
    set.seed(1)
    wordcloud2(wordcount2,size = input$Rotate,shape = input$Shape)
    
  })
  output$preface <-renderPrint({
    
    cat(sprintf("\nDr.  Kartikeya Bolar\n"))
    cat(sprintf("\nAssociate Professor and Area  Co-Chair\n"))
    cat(sprintf("\nOperations and Information Science\n"))
    cat(sprintf("\nT A  Pai Management Institute\n"))
    
  })
}

shinyApp(ui,server)