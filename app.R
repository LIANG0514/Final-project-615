rsconnect::setAccountInfo(name='iamsharkbait',token='0C27FFEC58B63E5DDA5710C3A5E8E885',secret='<SECRET>')
library(rsconnect)
library(XML)

library(shiny)

library(ggplot2)
library(sp)
library(ggmap)
library(tidyverse)
library(maptools)
library(datasets)
library(tigris)
library(plotly)
library(devtools)
library(twitteR)
library(tm)
library(stringr)
library(wordcloud)
library(gridExtra)
library(leaflet)
#devtools::install_github('ropensci/plotly')
people.tweets <- readRDS('player.tweets.rds')
eee<-readRDS('player.sentiment.rds')
KateUpton<-readRDS("tweets_kateupton.rds", refhook = NULL)
JustinVerlander<-readRDS("tweets_JVerlander.rds", refhook = NULL)
wed<-readRDS("tweets_wedding.rds")


###WORDCLOUD---Kate
wordCorpus1 <- Corpus(VectorSource(str_replace_all(KateUpton$text, "@", "")))
wordCorpus1 <- tm_map(wordCorpus1, removePunctuation)
wordCorpus1 <- tm_map(wordCorpus1, content_transformer(tolower))
wordCorpus1 <- tm_map(wordCorpus1, removeWords, stopwords("english"))
wordCorpus1 <- tm_map(wordCorpus1, removeWords, c("kate","upton","kateupton","fuck"))
wordCorpus1 <- tm_map(wordCorpus1, stripWhitespace)
###WORDCLOUD---Justin
wordCorpus2 <- Corpus(VectorSource(str_replace_all(JustinVerlander$text, "@", "")))
wordCorpus2 <- tm_map(wordCorpus2, removePunctuation)
wordCorpus2 <- tm_map(wordCorpus2, content_transformer(tolower))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, stopwords("english"))
wordCorpus2 <- tm_map(wordCorpus2, removeWords, c("justin", "verlander", "justinverlander"))
wordCorpus2 <- tm_map(wordCorpus2, stripWhitespace)

###SENTIMENT
k<-as.data.frame(eee[1])
j<-as.data.frame(eee[2])
colnames(k)<-c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative","Positive")
colnames(j)<-c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative","Positive")

k1<-reshape::melt(data = k, measure.vars = c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative","Positive"))

k2 = k1%>%
  select(variable, value)%>%
  group_by(variable)%>%
  summarise(count=sum(value))

j1<-reshape::melt(data = j, measure.vars = c("Anger","Anticipation","Disgust","Fear","Joy","Sadness","Surprise","Trust","Negative","Positive"))

j2 = j1%>%
  select(variable, value)%>%
  group_by(variable)%>%
  summarise(count=sum(value))






# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel( tabPanel("Inrtroduction",
                                      
                                      h5("This is Ang Li's 615 Final Project."),
                                      hr(),
                                      h2("The wedding on twitter"),
                                      p("Model Kate Upton and Houston Astros player Justin Verlander got married on November 4th, 2017. 
                                        The famous supermodel and pitcher finally got togather at an old church at the Rosewood Castiglion Del Bosco resort in Italy.
                                        Although they kept their wedding 'small', celebrities attended it, media reported it, 
                                        so it still became a hot topic on the social media platform, twitter.In this project, 
                                        I am going to try to find and study what people are talking about on twitter. "),
                                      img(src="kate5.png",height=500, width=520)), 
                             
                             tabPanel("WordCloud", 
                                      sidebarPanel(
                                        selectInput(
                                          inputId = "wordinput",
                                          label = "Choose a Person",
                                          choices = c("Kate","Justin")
                                                   ),
                                        h4("These are the wordclouds for twitters that talk about Kate and Justin. ")),
                                      mainPanel(
                                        
                                        plotOutput(outputId = "wordcloud"),
                                        textOutput("wordcloudtext")
                                               )), 
                             
                             tabPanel("Sentiment Analysis", 
                                      mainPanel(
                                        plotOutput(outputId = "senti"),
                                        textOutput("sentimenttext")), 
                                      sidebarPanel(
                                        selectInput(
                                          inputId = "sentiinput",
                                          label = "Choose a Person",
                                          choices = c("Kate","Justin")
                                                    )
                                                  )),
                             tabPanel("Map", 
                                      mainPanel(
                                                 leafletOutput("map",height = "550px")
                                               ),
                                      sidebarPanel(textOutput("maptext"))
                                      )
                             
                             
))








# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$wordcloud = renderPlot({
    if(input$wordinput=="Kate"){
    set.seed(100)
    wordcloud(words = wordCorpus1,scale=c(5,0.2),max.words=100, random.order=FALSE, 
              rot.per=0.15,use.r.layout=FALSE, colors=brewer.pal(5,"Greens")[4:8])
    }
    
    else{
      set.seed(100)
      wordcloud(words = wordCorpus2,scale=c(5,0.2),max.words=200, random.order=FALSE, 
                rot.per=0.2, use.r.layout=FALSE, colors=brewer.pal(5,"Greens")[4:8])
    }
  })
  
  output$wordcloudjustin = renderPlot({
    
  })
  
  
  output$senti = renderPlot({
    if(input$sentiinput=="Kate"){
    
    ggplot(k2, aes(variable, count))+
      geom_col(fill="tomato3")+
      ggtitle("Sentiment Plot for Kate") + 
      theme(plot.title = element_text(lineheight=.8, face="bold"),axis.text.x = element_text(angle = 45,hjust = 1,size=9))
    }
    
    else{
      ggplot(j2, aes(variable, count))+
        geom_col(fill="tomato3")+
        ggtitle("Sentiment Plot for Justin") + 
        theme(plot.title = element_text(lineheight=.8, face="bold"),axis.text.x = element_text(angle = 45,hjust = 1,size=9))
    }
    })
  
  
  
  output$map <- renderLeaflet({
    wed$longitude<-as.numeric(wed$longitude)
    wed$latitude<-as.numeric(wed$latitude)
    leaflet(data = wed)%>%
      addTiles()%>%
      addCircleMarkers(~wed$longitude,~wed$latitude)
  })
  
  
  output$timeline = renderPlotly({
    
    ku <- as.data.frame(people.tweets$KateUpton)
    p1<-plot_ly(ku, x = ~created, y = ~favoriteCount, name="Favorite", type = 'scatter',
                  mode = 'lines', line=list(color="red")) %>% add_trace(y=~retweetCount, name="Retweet", type = 'scatter', mode = 'lines', line=list(color="darkred")) %>% add_trace(
                    y=max(ku$favoriteCount, ku$retweetCount), type = 'scatter', mode = 'lines', line=list(color="white"), hoverinfo = "text", text = ~text) %>% layout(
                      title = 'Favorites/Retweets of Spotify Twitter Account in the Past 3 Years', xaxis = list(title = 'Date'), 
                      yaxis=list(title='Number of favorites/retweets'))
    p1$elementId <- NULL
    p1
    jv <- as.data.frame(people.tweets$JustinVerlander)
    p2<-plot_ly(jv, x = ~created, y = ~favoriteCount, name="Favorite", type = 'scatter',
            mode = 'lines', line=list(color="red")) %>% add_trace(y=~retweetCount, name="Retweet", type = 'scatter', mode = 'lines', line=list(color="navy")) %>% add_trace(
              y=max(jv$favoriteCount, jv$retweetCount), type = 'scatter', mode = 'lines', line=list(color="white"), hoverinfo = "text", text = ~text) %>% layout(
                title = 'Favorites/Retweets of Spotify Twitter Account in the Past 3 Years', xaxis = list(title = 'Date'), 
                yaxis=list(title='Number of favorites/retweets'))
    p2$elementId<-NULL
    p2
  })
  
    
  output$wordcloudtext = renderText({
    
      
      "For Kate's wordcloud, for the plot we can see that, except those words that describe or
      compliment Kate herself, such as 'amazing', 'gorgeous', the top 3 most frequently mentioned words are 'Justin', 'Verlander' and 
      'wedding'; For Justin, except for those words like 'Astros', which is the name of his team, the top 3
frequently mentioned words are 'Kate', 'Upton' and 'wedding'. It shows that on twitter, people are definitely talking about their relationship, and their wedding. "
    
    
  })
  
  
  output$sentimenttext = renderText({
    if (input$sentiinput=="Kate"){
      "From the graph of sentiment for Kate, we can see that the word that related with positive emotions are greatly
      exceed negative emotions, especially for emotions like 'Joy', 'Trust', 'Anticipation'. "
    }
    
    else{
      
      "Similar with the graph of Kate, from the graph of sentiment for Justin, we see that the word that related with positive emotions are greatly
      exceed negative emotions (I notice negative emotions uprise a little bit), and emotions like 'Joy', 'Trust', 'Anticipation' remains high in our sample. "
    }
  })
  
  output$maptext = renderText({
    "This is the map that shows the location of people who have interaction with kate's and Justin's twiiter account. 
    Due to the privacy policy of twitter, I cannot obtain the geographic locations of all the people. 
    However, we can still find something from even these a few points. Those points are spotted on several continents, such as 
    North America,Europe or Oceania. It means that Kate and Justin's wedding are discussed and concerned worldwidely, which means this topic is popular and well-known
    for recent days. "
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

