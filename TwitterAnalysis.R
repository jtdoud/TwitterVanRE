
# Name:                 Twitter Word Cloud
# Created by:           Tucker Doud
# Modified Date:        Mar 4, 2016
# Data Source:          Twitter

# https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/wordcloud1

setwd("P:/Research and Analytics/Projects/2015/TwitterAnalysis")
# Load tools
library(twitteR)
library(tm)
library(wordcloud)

# Get Tweets ----------------------------------------------------------

apiKey <- "xxx"
apiSecret <- "xxx"
token <- "xxx"
tokSecret <- "xxx"

setup_twitter_oauth(apiKey, apiSecret, token, tokSecret)
rm(apiKey, apiSecret, token, tokSecret)
tweets <- searchTwitter("uwlm", n= 500)
#tweets <- strip_retweets(tweets)
txt <- sapply(tweets, function(x) x$getText()) #extracts text only
#tweetDF <- twListToDF(tweets)

# Do some cleaning
txt <- iconv(txt, "latin1", "ASCII", sub = "") #Need this to allow tolower to work on emojis
txt <- gsub("http(s?)([^ ]*)", " ", txt, ignore.case = T) #Remove links
txt <- gsub("&amp", "and", txt) #remove html '&amp'

# Text Mining ----------------------------------------------------------

corp <- Corpus(VectorSource(txt)) #Create corpus
moreStopWords <- c("united", "way", "uwlm", "mainland")

# Create a document term matrix
ctrl <- list(removePunctuation= list(preserve_intra_word_dashes = T),
             tolower= T,
             stopwords= c(stopwords(kind = "en"), moreStopWords),
             removeNumbers= F)
             #stemming = T) #Need SnowballC package for this to work
tdm <- TermDocumentMatrix(x= corp, control= ctrl)
findFreqTerms(tdm, 20)
# findAssocs(tdm, "foreign", 0.4)

wrdFreqs <- sort(rowSums(as.matrix(tdm)), decreasing= T)
wrdFreqsDF <- data.frame(word= names(wrdFreqs), freq= wrdFreqs, stringsAsFactors= F, 
                 row.names= NULL)
rm(wrdFreqs)

# Stats on word freqs
summary(wrdFreqsDF$freq)

# Visuals --------------------------------------------------------------
my_png <- function(dest) {
    png(filename=dest, width=25.4, height=19.05, units="cm", res=300)
}

my_png("./uwlmWordCloud.png")
print(
wordcloud(words= wrdFreqsDF$word, freq= wrdFreqsDF$freq, scale = c(3, 0.5), 
          random.order= F, min.freq= 5, colors= brewer.pal(8, "Dark2"))
)
dev.off()