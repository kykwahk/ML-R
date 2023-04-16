
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

#########################################
## 제7장 텍스트마이닝 - 7.5 토픽모델링 ##
#########################################

## 사례: 뉴스기사

library(textdata)
news <- dataset_ag_news(split="test")
news
table(news$class)

library(tm)
docs <- VCorpus(VectorSource(news$description))
docs
lapply(docs, content)[1:3]

docs <- tm_map(docs, content_transformer(tolower))
myRemove <- function(x, pattern)
  {return(gsub(pattern, "", x))}
docs <- tm_map(docs, content_transformer(myRemove), "(f|ht)tp\\S+\\s*")
docs <- tm_map(docs, content_transformer(myRemove), "www\\.+\\S+")
mystopwords <- c(stopwords("english"),
                 c("first", "second", "one", "two", "three", "four", "another", 
                   "last", "least", "just", "will", "week", "weeks","quot", 
                   "ago", "day", "days", "night", "nights", "month","months", 
                   "years", "year", "next", "now", "today", "yesterday", 
                   "may", "new", "york", "according", "back", "say","says", 
                   "said", "can", "make","made", "reuters", "monday", "tuesday", 
                   "wednesday", "thursday", "friday", "saturday", "sunday"))
docs <- tm_map(docs, removeWords, mystopwords)
toSpace <- function(x, pattern)
  {return(gsub(pattern, " ", x))}
docs <- tm_map(docs, content_transformer(toSpace), ":")
docs <- tm_map(docs, content_transformer(toSpace), ";")
docs <- tm_map(docs, content_transformer(toSpace), "/")
docs <- tm_map(docs, content_transformer(toSpace), "\\.")
docs <- tm_map(docs, content_transformer(toSpace), "\\\\")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(trimws))
lapply(docs, content)[1:3]

dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- paste0(rownames(dtm), "-", news$class)
inspect(dtm)

library(slam)
summary(col_sums(dtm))

col_sums(dtm)[order(col_sums(dtm), decreasing=TRUE)][1:5]
findFreqTerms(dtm, lowfreq=250)

library(topicmodels)
news.lda <- LDA(dtm, k=4, method="Gibbs", 
                control=list(seed=123, burnin=1000, iter=1000, thin=100))
class(news.lda)

topics(news.lda)[1:5]

table(topics(news.lda))

terms(news.lda, 10)

str(news.lda, max.level=2, nchar.max=50)

news.lda@beta[, 1:5]
exp(news.lda@beta[, 1:5])

library(tidytext)
news.term <- tidy(news.lda, matrix="beta")
news.term

library(dplyr)
news.term.top <- news.term %>%
  group_by(topic) %>%
  slice_max(order_by=beta, n=5) %>%
  ungroup() %>%
  arrange(topic, -beta)
news.term.top

# [그림 7-24]
windows(width=7.0, height=5.5)
library(ggplot2)
ggplot(news.term.top, 
       aes(reorder_within(x=term, by=beta, within=topic), 
           beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ paste("Topic", topic), scales="free") +
  scale_x_reordered() +
  coord_flip() +
  theme_minimal() +
  labs(x=NULL, y="Word-Topic Probability (Beta)",
       title="News") +
  theme(plot.title=element_text(face="bold"),
        strip.text=element_text(face="bold"))

news.lda@gamma[1:5, ]

news.doc <- tidy(news.lda, matrix="gamma")
news.doc

library(tidyr)
news.doc <- news.doc %>%
  separate(document, c("id", "category"), sep="-", convert=TRUE)
news.doc

# [그림 7-25]
windows(width=7.0, height=5.5)
library(ggplot2)
ggplot(news.doc, aes(factor(topic), gamma, fill=category)) +
  geom_boxplot(color="gray50", show.legend=FALSE,
               outlier.shape=21, outlier.color="black", outlier.fill="gray") +
  facet_wrap(~ factor(category), scales="free") +
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  labs(x="Topic", y="Document-Topic Probability (Gamma)",
       title="News") +
  theme(strip.background=element_rect(fill="aliceblue"),
        strip.text=element_text(face="bold", color="steelblue"),
        plot.title=element_text(face="bold"),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank())
