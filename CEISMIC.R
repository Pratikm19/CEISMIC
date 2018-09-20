#CEISMIC code

head(CEI)
library(tm)
corp<-Corpus(VectorSource(CEI))
corp <- tm_map(corp, tolower)
corp <- tm_map(corp, removePunctuation)
# remove numbers
corp <- tm_map(corp, removeNumbers) 
corp<-tm_map(corp,PlainTextDocument)
# remove stop words like: and, the, is, etc. 
corp<-tm_map(corp,removeWords, stopwords("english"))
corp<-tm_map(corp,removeWords,c("just","got","get","like"))

#display a particular document from corpus 
writeLines(as.character(corp[[1]]))
inspect(corp[1:3])
dtm <- TermDocumentMatrix(corp)
m <- as.matrix(dtm)
findFreqTerms(dtm)
findFreqTerms(dtm,100)
findAssocs(dtm, 'earthquake', 0.30)

v <- sort(rowSums(m), decreasing=TRUE) 
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(d$word, d$freq,random.order = FALSE , min.freq=1, colors=pal2)

term.freq <- rowSums(m)
term.freq <- subset(term.freq, term.freq >= 10)
#type term.freq and see the object 
term.freq
#make a data frame using the object ter,.freq
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()


# package for sentimental analysis 
install.packages(“RSentiment”) 
library(RSentiment)
#import text file with one text record in one row 
data<-readLines(file.choose())
head(data)
# calculate sentiment score of each document 
calculate_score(data)







 