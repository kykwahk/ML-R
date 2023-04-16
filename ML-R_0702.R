
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

#######################################
## 제7장 텍스트마이닝 - 7.2 빈도분석 ##
#######################################

## 사례: 대통령 연설문

library(quanteda)
summary(data_corpus_inaugural)
class(data_corpus_inaugural)

library(tidytext)
library(tibble)
library(dplyr)
us.president.address <- tidy(data_corpus_inaugural) %>% 
  group_by(President, FirstName) %>% 
  summarise(across(.col=everything(), .fns=~paste(.x, collapse=" "))) %>% 
  arrange(Year) %>% 
  ungroup() %>% 
  slice_tail(n=4)  
us.president.address

us.president.address <- us.president.address %>% 
  select(text, everything()) %>% 
  add_column(doc_id=1:nrow(.), .before=1) 
us.president.address
library(tm)
address.corpus <- VCorpus(DataframeSource(us.president.address))
address.corpus

address.corpus <- tm_map(address.corpus, content_transformer(tolower))
mystopwords <- c(stopwords("english"), c("can", "must", "will"))
address.corpus <- tm_map(address.corpus, removeWords, mystopwords)
address.corpus <- tm_map(address.corpus, removePunctuation)
address.corpus <- tm_map(address.corpus, removeNumbers)
address.corpus <- tm_map(address.corpus, stripWhitespace)
address.corpus <- tm_map(address.corpus, content_transformer(trimws))
lapply(address.corpus, content)

library(stringr)
str_count(string=paste(as.vector(unlist(lapply(address.corpus, content))), 
                       collapse=" "), 
          pattern="america")
for (i in c("america", "americas", "american", "americans")) {
  n <- str_count(string=paste(as.vector(unlist(lapply(address.corpus, content))), 
                              collapse=" "), pattern=paste0("\\b", i, "\\b"))
  print(paste(i, n))
}
address.corpus <- tm_map(address.corpus, content_transformer(gsub), 
                         pattern="america|americas|american|americans", 
                         replacement="america")
lapply(address.corpus, content)

address.dtm <- DocumentTermMatrix(address.corpus)
inspect(address.dtm)

term.freq <- colSums(as.matrix(address.dtm))

head(term.freq)
length(term.freq)

term.freq[head(order(term.freq, decreasing=TRUE))]
term.freq[tail(order(term.freq, decreasing=TRUE))]

findFreqTerms(address.dtm, lowfreq=40)
findFreqTerms(address.dtm, lowfreq=50, highfreq=100)

term.freq.df <- data.frame(word=names(term.freq), frequency=term.freq)
head(term.freq.df)

# [그림 7-2]
windows(width=7.0, height=5.5)
library(ggplot2)
ggplot(subset(term.freq.df, frequency >= 30), 
       aes(x=reorder(word, frequency), y=frequency, fill=word)) + 
  geom_col(color="dimgray", width=0.6, show.legend=FALSE) +
  geom_text(aes(label=frequency), size=3.5, color="black", hjust=-0.3) +
  coord_flip() +
  labs(x=NULL, y="Term Frequency (count)",
       title="American President's Inaugural Address") + 
  theme(plot.title=element_text(face="bold"))

# [그림 7-3]
windows(width=6, height=6)
set.seed(123)
library(wordcloud)
library(RColorBrewer)
term.freq <- colSums(as.matrix(address.dtm))
wordcloud(words=names(term.freq), freq=term.freq, scale=c(4, 0.5), min.freq=7,
          rot.per=0.1, random.order=FALSE, random.color=FALSE, 
          colors=brewer.pal(6, "Dark2"))

inspect(address.dtm)
rownames(address.dtm) <- us.president.address$President
Docs(address.dtm)

address.tf <- tidy(address.dtm)
address.tf

address.tf <- address.tf %>% 
  mutate(document=factor(document, levels=us.president.address$President)) %>% 
  arrange(desc(count)) %>% 
  group_by(document) %>% 
  slice_max(order_by=count, n=10) %>% 
  ungroup() 
address.tf

# [그림 7-4]
windows(width=7.0, height=5.5)
library(ggplot2)
ggplot(address.tf, aes(reorder_within(x=term, by=count, within=document), 
                       count, fill=document)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~ document, ncol=2, scales="free") +
  scale_x_reordered() +
  coord_flip() +
  labs(x=NULL, y="Term Frequency (count)",
       title="American President's Inaugural Address") +
  theme(plot.title=element_text(face="bold"))

address.dtm2 <- DocumentTermMatrix(address.corpus, 
                                   control=list(weighting=weightTfIdf))
rownames(address.dtm2) <- us.president.address$President
inspect(address.dtm2)

address.tfidf <- tidy(address.dtm2) %>% 
  mutate(tf_idf=count, count=NULL)
address.tfidf
address.tfidf <- address.tfidf %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(document=factor(document, levels=us.president.address$President)) %>% 
  group_by(document) %>% 
  slice_max(order_by=tf_idf, n=10) %>% 
  ungroup() 
address.tfidf

# [그림 7-5]
windows(width=7.0, height=9.0)
library(ggplot2)
ggplot(address.tfidf, aes(reorder_within(x=term, by=tf_idf, within=document), 
                          tf_idf, fill=document)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~ document, ncol=2, scales="free") +
  scale_x_reordered() +
  coord_flip() +
  labs(x=NULL, y="Term Frequency-Inverse Document Frequency",
       title="American President's Inaugural Address") +
  theme(plot.title=element_text(face="bold"))

library(quanteda)
library(tidytext)
library(tibble)
library(dplyr)
us.president.address <- tidy(data_corpus_inaugural) %>% 
  group_by(President, FirstName) %>% 
  summarise(across(.col=everything(), .fns=~paste(.x, collapse=" "))) %>% 
  arrange(Year) %>% 
  ungroup() %>% 
  slice_tail(n=4) %>% 
  select(text, everything()) %>% 
  add_column(doc_id=1:nrow(.), .before=1) 
us.president.address

address.words <- us.president.address %>% 
  unnest_tokens(word, text) 
address.words

address.words <- address.words %>% 
  anti_join(stop_words, by="word") %>% 
  filter(!grepl(pattern="\\d+", word)) %>% 
  mutate(word=gsub(pattern="'|’", replacement="", word)) %>%
  mutate(word=gsub(pattern="america|americas|american|americans",
                   replacement="america", word)) %>%
  count(President, word, sort=TRUE, name="count") %>% 
  ungroup()
address.words

# [그림 7-6]
windows(width=7.0, height=5.5)
library(ggplot2)
address.words %>% 
  group_by(word) %>%  
  summarize(count=sum(count)) %>% 
  arrange(desc(count)) %>% 
  slice_max(order_by=count, n=10) %>% 
  ggplot(aes(reorder(word, -count), count)) +
  geom_col(color="dimgray", fill="salmon", width=0.6, show.legend=FALSE) +
  geom_text(aes(label=count), size=3.5, color="black", vjust=-0.5) +
  labs(x=NULL, y="Term Frequency (count)",
       title="American President's Inaugural Address") +
  theme(plot.title=element_text(face="bold"),
        axis.text.x=element_text(angle=45, hjust=1))

address.words <- address.words %>% 
  bind_tf_idf(term=word, document=President, n=count) 

address.words %>%
  arrange(desc(tf_idf))
address.words %>% 
  arrange(tf_idf)

# [그림 7-7]
windows(width=7.0, height=7.0)
library(ggplot2)
address.words %>%
  arrange(desc(tf_idf)) %>% 
  mutate(President=factor(President, levels=us.president.address$President)) %>% 
  group_by(President) %>% 
  slice_max(order_by=tf_idf, n=7) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(x=word, by=tf_idf, within=President), 
             tf_idf, fill=President)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~ President, ncol=2, scales="free") +
  scale_x_reordered() +
  coord_flip() +
  labs(x=NULL, y="Term Frequency-Inverse Document Frequency",
       title="American President's Inaugural Address") +
  theme(plot.title=element_text(face="bold"))

# [그림 7-8]
windows(width=7.0, height=5.5)
library(ggplot2)
address.words %>%
  arrange(desc(tf)) %>% 
  mutate(President=factor(President, levels=us.president.address$President)) %>%
  group_by(President) %>% 
  slice_max(order_by=tf, n=7) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(x=word, by=tf, within=President), 
             tf, fill=President)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~ President, ncol=2, scales="free") +
  scale_x_reordered() +
  coord_flip() +
  labs(x=NULL, y="Term Frequency (proportion)",
       title="American President's Inaugural Address") +
  theme(plot.title=element_text(face="bold"))
