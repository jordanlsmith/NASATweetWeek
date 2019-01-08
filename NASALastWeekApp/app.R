library(shiny)
library(shinydashboard)
library(rtweet)
library(tidyverse)
library(syuzhet)

## get most recent 1000 tweets posted by NASA's account
nasa <- get_timeline("NASA", n = 1000)

## filter for last seven days of tweets
lastweek <- nasa %>%
  filter(created_at > (Sys.time() -(7*60*60*24))) #here, subtracting 7 days time from user's current datetime

##top hashtag
#define function
extract.hashes = function(vec){
  hash.pattern = "#[[:alpha:]]+"
  have.hash = grep(x = vec, pattern = hash.pattern)
  hash.matches = gregexpr(pattern = hash.pattern,
                          text = vec[have.hash])
  extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)

  df = data.frame(table(tolower(unlist(extracted.hash))))
  colnames(df) = c("tag","freq")
  df = df[order(df$freq,decreasing = TRUE),]
  return(df)
} #end extract.hashes function
#get hashtag data
hashtable <- transform(extract.hashes(lastweek$text),tag = reorder(tag,freq))

## sentiment analysis
#first, clean text
tweets <- lastweek$text
#get rid of unnecessary spaces
clean_tweet <- str_replace_all(tweets," "," ")
# Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http(s?)://t.co/[a-z,A-Z,0-9]*","")
# Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")
# Get rid of &
clean_tweet = gsub("&amp", "", clean_tweet)
# Get rid of punctuation
clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
# Get rid of digits
clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
# get sentences to define corpus
corpus <- get_sentences(clean_tweet)
#get sentiment of corpus
sentiment <- get_sentiment(corpus)
#make text sentiment df
text_sentiment <- as.data.frame(cbind(lastweek$text, sentiment), stringsAsFactors = F)


ui <- dashboardPage(
  dashboardHeader(title = "Tweet Week: @NASA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", 
               tabName = "Summary", 
               icon = icon("dashboard")),
      menuItem("Tweet Highlights", 
               icon = icon("comment"), 
               tabName = "Highlights",
               badgeColor = "green"),
      menuItem("Sentiment Analysis", 
             icon = icon("smile-o"), 
             tabName = "Sentiment",
             badgeColor = "yellow")
       ) #end sidebarMenu
      ), #end dashboardSidebar 
  dashboardBody(
    tabItems(
      # Summary tab content
      tabItem(tabName = "Summary",
              fluidRow(
                valueBoxOutput("totalTweetsBox", width = 6),
                valueBoxOutput("topHashtagBox", width = 6)
              ), #end fluidRow
              fluidRow(
                valueBoxOutput("mostFavoritesBox", width = 6),
                valueBoxOutput("mostRetweetsBox", width = 6)
              ), #end fluidRow
              fluidRow(
                box(title = "Frequency of @NASA tweets from past 7 days",
                    plotOutput("timelinePlot"), width = 12, color = "blue"
                ) #end box
              ) #end fluidRow
      ), #end tabItem
      
      # Highlight tab content
      tabItem(tabName = "Highlights",
              fluidRow(
                valueBoxOutput("favoriteTweetTextBox", width = 12)
              ), #end fluidRow
              fluidRow(
                valueBoxOutput("mostRetweetTextBox", width = 12)
              ), #end fluidRow
              fluidRow(
                valueBoxOutput("mostPositiveTextBox", width = 12)
              ), #end fluidRow
              fluidRow(
                valueBoxOutput("mostNegativeTextBox", width = 12)
              ) #end fluidRow
      ), #end tabItem
      # Sentiment tab content
      tabItem(tabName = "Sentiment",
              fluidRow(
                valueBoxOutput("totalNegativesBox"),
                valueBoxOutput("totalNeutralsBox"),
                valueBoxOutput("totalPositivesBox")
              ), #end fluidRow
              fluidRow(
                box(title = "Sentiment of @NASA tweets from past 7 days",
                    plotOutput("sentimentPlot"), width = 12
                ) #end box
              )
      ) #end tabItem
    ) #end tabItems
  ) #end dashboardBody
) #end dashboardPage

#server function
server <- function(input, output) { 
  output$timelinePlot <- renderPlot(
    ts_plot(lastweek, "3 hours") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      ggplot2::labs(
        x = NULL, y = NULL,
        #title = "Frequency of @NASA tweets from past 7 days",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
      ) #end labs
  ) #end renderPlot
  
  output$sentimentPlot <- renderPlot(
    ggplot(data = text_sentiment, aes(as.numeric(sentiment))) +
      geom_histogram(binwidth=0.25) +
      xlim(c(-3,3)) +
      theme_minimal() +
      labs(x = "Tweet Sentiment", y = "Count",
           caption = "\nSource: Data collected from Twitter's REST API via rtweet"
           ) #end labs
  ) #end renderPlot
  
  #summary Boxes
  output$totalTweetsBox <- renderValueBox({
    valueBox(
      subtitle = "Number of Tweets",
      value = nrow(lastweek),
      icon = icon("twitter"),
      color = "blue"
    )
  })
  
  output$topHashtagBox <- renderValueBox({
    valueBox(
      subtitle = "Top Hashtag",
      value = as.character(hashtable$tag[1]),
      icon = icon("hashtag"),
      color = "blue"
    )
  })
  
  output$mostFavoritesBox <- renderValueBox({
    valueBox(
      subtitle = "Most Favorites",
      value = max(lastweek$favorite_count),
      icon = icon("star"),
      color = "blue"
    )
  })
  
  output$mostRetweetsBox <- renderValueBox({
    valueBox(
      subtitle = "Most Retweets",
      value = max(lastweek$retweet_count),
      icon = icon("retweet"),
      color = "blue"
    )
  })
  
  #Sentiment boxes
  output$totalNegativesBox <- renderValueBox({
    valueBox(
      subtitle = "Negative Tweets",
      value = length(corpus[sentiment < 0]),
      icon = icon("frown-o"),
      color = "red"
    )
  })
  
  output$totalNeutralsBox <- renderValueBox({
    valueBox(
      subtitle = "Neutral Tweets",
      value = length(corpus[sentiment == 0]),
      icon = icon("meh-o"),
      color = "yellow"
    )
  })
  
  output$totalPositivesBox <- renderValueBox({
    valueBox(
      subtitle = "Positive Tweets",
      value = length(corpus[sentiment > 0]),
      icon = icon("smile-o"),
      color = "green"
    )
  })
  
  ## Highlights
  output$favoriteTweetTextBox <- renderValueBox({
    valueBox(
      value = "Most Favorited Tweet",
      subtitle = lastweek$text[lastweek$favorite_count == max(lastweek$favorite_count)][1],
      icon = icon("star"),
      color = "blue"
    )
  })
  
  output$mostRetweetTextBox <- renderValueBox({
    valueBox(
      value = "Most Retweeted Tweet",
      subtitle = lastweek$text[lastweek$retweet_count == max(lastweek$retweet_count)][1],
      icon = icon("retweet"),
      color = "blue"
    )
  })
  
  output$mostPositiveTextBox <- renderValueBox({
    valueBox(
      value = "Most Positive Tweet",
      subtitle = as.character(text_sentiment$V1[sentiment == max(sentiment)][1]),
      icon = icon("smile-o"),
      color = "green"
    )
  })
  
  output$mostNegativeTextBox <- renderValueBox({
    valueBox(
      value = "Most Negative Tweet",
      subtitle = as.character(text_sentiment$V1[sentiment <= min(sentiment)][1]),
      icon = icon("frown-o"),
      color = "red"
    )
  })
  
  # output$mostfavorited <- lastweek$text[lastweek$favorite_count == max(lastweek$favorite_count)][1]
  # output$leastfavorited <- lastweek$text[lastweek$favorite_count == min(lastweek$favorite_count)][1]
  # output$mostretweeted <- lastweek$text[lastweek$retweet_count == max(lastweek$retweet_count)][1]
  # output$leastretweeted <- lastweek$text[lastweek$retweet_count == min(lastweek$retweet_count)][1]

  
  # #find most positive tweet
  # output$most_positive <- corpus[sentiment == max(sentiment)][1]
  # #find most negative tweet
  # output$least_positive <- corpus[sentiment <= min(sentiment)]
  # #define sentiment type counts
  # output$positive_tweets <- nrow(corpus[sentiment > 0])
  # output$negative_tweets <- nrow(corpus[sentiment < 0])
  # output$neutral_tweets <- nrow(corpus[sentiment == 0])
  # 
  } #end server function

# Preview the UI in the console
shinyApp(ui, server)