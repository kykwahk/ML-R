
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

#######################################
## 제7장 텍스트마이닝 - 7.4 분류분석 ##
#######################################

## 사례: 스팸 필터링

library(readr)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip"
local.copy <- tempfile()
download.file(url, local.copy, mode="wb")
sms <-  read_delim(unzip(zipfile=local.copy, files="SMSSpamCollection"),
                   delim="\t", quote="", 
                   col_types=cols("f", "c"),
                   col_names=c("type", "text"))
unlink(local.copy)
sms

table(sms$type)
prop.table(table(sms$type))

library(dplyr)
library(tibble)
library(tm)
sms <- sms %>% 
  select(text, type) %>% 
  add_column(doc_id=1:nrow(.), .before=1) %>% 
  mutate(text=iconv(text, to="ascii", sub=""))
sms
docs <- VCorpus(DataframeSource(sms))
docs

lapply(docs, content)[c(13, 16, 20)]
meta(docs)$type[c(13, 16, 20)]

docs <- tm_map(docs, content_transformer(tolower))

myRemove <- function(x, pattern)
  {return(gsub(pattern, "", x))}
docs <- tm_map(docs, content_transformer(myRemove), "(f|ht)tp\\S+\\s*")
docs <- tm_map(docs, content_transformer(myRemove), "www\\.+\\S+")
lapply(docs, content)[c(13, 16, 20)]

mystopwords <- c(stopwords("english"),
                 c("can", "cant", "don", "dont", "get", "got", "just", "one", "will"))
docs <- tm_map(docs, removeWords, mystopwords)
lapply(docs, content)[c(13, 16, 20)]

toSpace <- function(x, pattern)
  {return(gsub(pattern, " ", x))}
docs <- tm_map(docs, content_transformer(toSpace), ":")
docs <- tm_map(docs, content_transformer(toSpace), ";")
docs <- tm_map(docs, content_transformer(toSpace), "/")
lapply(docs, content)[c(13, 16, 20)]

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(trimws))
docs <- tm_map(docs, stemDocument)
lapply(docs, content)[c(13, 16, 20)]

dtm <- DocumentTermMatrix(docs)
dtm
inspect(dtm)

dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(4, 10),
                                             bounds=list(global=c(5, 5300))))
dtm

term.freq <- colSums(as.matrix(dtm))
term.freq[head(order(term.freq, decreasing=TRUE))]
term.freq[tail(order(term.freq, decreasing=TRUE))]

findFreqTerms(dtm, lowfreq=200)

findAssocs(dtm, terms=c("call", "free"), corlimit=c(0.20, 0.25))

# [그림 7-15]
windows(width=5.5, height=5.5)
library(wordcloud)
library(RColorBrewer)
set.seed(123)
wordcloud(words=names(colSums(as.matrix(dtm))), freq=colSums(as.matrix(dtm)), 
          scale=c(4, 0.5), min.freq=30, max.words=200,
          rot.per=0, random.order=FALSE, random.color=FALSE, 
          colors=brewer.pal(6, "Set2"))

hamspam <- as.matrix(dtm)
rownames(hamspam) <- sms$type
hamspam <- rowsum(hamspam, group=rownames(hamspam))
hamspam[, 1:10]

# [그림 7-16]
windows(width=5.5, height=5.5)
library(wordcloud)
set.seed(123)
comparison.cloud(t(hamspam), colors=c("cornflowerblue", "tomato"), title.size=2, 
                 title.colors=c("blue", "red"), title.bg.colors=c("wheat"), 
                 rot.per=0, scale=c(5, 0.4), max.words=200, match.colors=TRUE)

set.seed(123)
train <- sample(nrow(sms), 0.7*nrow(sms))
y.train <- sms[train,]$type
y.test <- sms[-train,]$type
table(y.train)
table(y.test)

prop.table(table(y.train))
prop.table(table(y.test))

library(caret)
train2 <- createDataPartition(sms$type, p=0.7, list=FALSE)
y.train2 <- sms[train2,]$type
y.test2 <- sms[-train2,]$type
table(y.train2)
table(y.test2)
prop.table(table(y.train2))
prop.table(table(y.test2))

toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels=c(0, 1), labels=c("no", "yes"))
  return(x)
}

sms.dtm <- apply(dtm, MARGIN=2, toFactor)
str(sms.dtm)

x.train <- sms.dtm[train,]
x.test <- sms.dtm[-train,]

library(e1071)
sms.nb <- naiveBayes(x=x.train, y=y.train)

sms.nb.pred <- predict(sms.nb, newdata=x.test)

table(y.test, sms.nb.pred, dnn=c("Actual", "predicted"))
mean(sms.nb.pred==y.test)
