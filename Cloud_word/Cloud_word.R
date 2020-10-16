#Step 1 : directory()
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#Ruta
ruta="E:/Jennyfer_C/00_R/Cloud_word"
setwd(ruta)
getwd()


#Step 2 : Install and load the required packages
# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Step 3 : Text mining

filePath<-"E:/Jennyfer_C/00_R/Cloud_word/speech.txt"
text <- readLines(filePath, warn=FALSE)

# Read the text file from internet
#filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
#text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

#Inspect the content of the document
inspect(docs)

#Text transformation
#Transformation is performed using tm_map() function to replace, for example, special characters from the text.
#Replacing "/", "@" and "|" with space:

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#Cleaning the text
#the tm_map() function is used to remove unnecessary white space, to convert the text to lower case, to remove common stopwords like 'the', "we".
#The information value of 'stopwords' is near zero due to the fact that they are so common in a language. Removing this kind of words is useful before further analyses. For 'stopwords', supported languages are danish, dutch, english, finnish, french, german, hungarian, italian, norwegian, portuguese, russian, spanish and swedish. Language names are case sensitive.

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#Step 4 : Build a term-document matrix
#Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. The function TermDocumentMatrix() from text mining package can be used as follow :

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Step 5 : Generate the Word cloud
#The importance of words can be illustrated as a word cloud as follow :

set.seed(1234)
mypalette <-brewer.pal(8,"Dark2")
wordcloud(words = d$word, freq = d$freq, scale=c(8,2), min.freq=1,rot.per=0.15, max.words=Inf, random.order=F, random.color=T, colors=mypalette)


library(wordcloud2)
#wordcloud2(data = demoFreq)

tdm<-TermDocumentMatrix(docs)
m<-as.matrix(tdm)
v<-sort(rowSums(m),decreasing = TRUE)
Docs2<-data.frame(word=names(v),freq =v)
head(Docs2)

wordcloud2(data = Docs2)

outputId<-wordcloud2(Docs2, color = "random-light",size = 0.5, shape = "circle", backgroundColor = "white") 
wordcloud2Output(outputId, width = "100%", height = "400px")

#install.packages("webshot")
#install.packages("htmlwidgets")
#install.packages("debugme")
#webshot::install_phantomjs()
library(webshot)
library(htmlwidgets)
library(debugme)

my_graph=wordcloud2(Docs2, color = "random-light",size = 0.5, shape = "circle", backgroundColor = "white") 
my_graph2=wordcloud2(Docs2, color = "random-light",size = 0.5, shape = "circle", backgroundColor = "white", ellipticity = 0.1)

#hw = wordcloud2(demoFreq,size = 3)
#saveWidget(hw,"1.html",selfcontained = F)
#webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)


saveWidget(my_graph,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)

saveWidget(my_graph2,"2.html",selfcontained = F)
webshot::webshot("2.html","2.png",vwidth = 1992, vheight = 1744, delay =10)


#install webshot
#install.packages("webshot")
library(webshot)
#webshot::install_phantomjs()

# Make the graph
my_graph=wordcloud2(demoFreq, size=1.5)

# save it in html
library("htmlwidgets")
saveWidget(my_graph,"tmp.html",selfcontained = F)
saveWidget(my_graph2,"tmp2.html",selfcontained = F)

# and in pdf
webshot("tmp.html","fig_1.pdf", delay =5, vwidth = 480, vheight=480)





color = "random-light")
mypalette <-brewer.pal(8,"Dark2")


png("wordcloud_lknd.png", width=1584,height=396)
wordcloud(words = d$word, freq = d$freq, scale=c(8,2), min.freq=1,rot.per=0.15, max.words=Inf, random.order=F, random.color=T, colors=mypalette)
dev.off()
