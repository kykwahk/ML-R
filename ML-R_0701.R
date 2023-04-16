
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

############################################
## 제7장 텍스트마이닝 - 7.1 텍스트 구조화 ##
############################################

## 코퍼스

library(tm)
data(crude)
crude

crude[[1]]$content
crude[[1]]$meta

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")

getSources()

corpus.docs <- VCorpus(VectorSource(text))
class(corpus.docs)

corpus.docs
inspect(corpus.docs[1])

inspect(corpus.docs[[1]])

as.character(corpus.docs[[1]])
lapply(corpus.docs, as.character)

str(corpus.docs[[1]])

corpus.docs[[1]]$content
lapply(corpus.docs, content)
paste(as.vector(unlist(lapply(corpus.docs, content))), collapse=" ")

corpus.docs[[1]]$meta
meta(corpus.docs[[1]])

meta(corpus.docs[[1]], tag="id")
meta(corpus.docs[[1]], tag="datetimestamp")

meta(corpus.docs[[1]], tag="author")
meta(corpus.docs[[1]], tag="author", type="local") <- "BBC"
meta(corpus.docs[[1]])

source <- c("BBC", "CNN", "FOX")
meta(corpus.docs, tag="author", type="local") <- source
lapply(corpus.docs, meta, tag="author")

category <- c("health", "lifestyle", "business")
meta(corpus.docs, tag="category", type="local") <- category
lapply(corpus.docs, meta, tag="category")

meta(corpus.docs, tag="origin", type="local") <- NULL
lapply(corpus.docs, meta)

corpus.docs.filtered <- 
  tm_filter(corpus.docs, FUN=function(x) any(grep("weight|diet", content(x))))
lapply(corpus.docs.filtered, content)

idx <- meta(corpus.docs, "author")=="FOX" | meta(corpus.docs, "category")=="health"
lapply(corpus.docs[idx], content)
lapply(corpus.docs[idx], meta)

writeCorpus(corpus.docs)
list.files(pattern="\\.txt")

getTransformations()

corpus.docs <- tm_map(corpus.docs, content_transformer(tolower))
lapply(corpus.docs, content)

stopwords("english")

corpus.docs <- tm_map(corpus.docs, removeWords, stopwords("english"))
lapply(corpus.docs, content)

mystopwords <- c(stopwords("english"), c("among", "beyond"))
mystopwords <- setdiff(mystopwords, c("own", "few"))
corpus.mydocs <- tm_map(corpus.docs, removeWords, mystopwords)

myRemove <- function(x, pattern)
  {return(gsub(pattern, "", x))}

corpus.docs <- tm_map(corpus.docs, content_transformer(myRemove), "(f|ht)tp\\S+\\s*")
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, removePunctuation)

corpus.docs <- tm_map(corpus.docs, removeNumbers)

corpus.docs <- tm_map(corpus.docs, stripWhitespace)
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, content_transformer(trimws))
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, stemDocument)
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, content_transformer(gsub), 
                      pattern="economist", replacement="economi")
lapply(corpus.docs[3], content)

## 타이디-텍스트

library(tm)
data(crude)
library(dplyr)
library(tidytext)
crude.tidy <- tidy(crude) %>% 
  select(c("datetimestamp", "heading", "text")) %>% 
  unnest_tokens(output=word, input=text)
crude.tidy

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
source <- c("BBC", "CNN", "FOX")

library(dplyr)
text.df <- tibble(source=source, text=text)
text.df
class(text.df)

library(tidytext)
tidy.docs <- text.df %>% 
  unnest_tokens(output=word, input=text)
tidy.docs

tidy.docs %>% print(n=Inf)

tidy.docs %>% 
  count(source) %>% 
  arrange(desc(n))

word.removed <- tibble(word=c("http", "bbc.in", "1g0j4agg"))
tidy.docs <- tidy.docs %>% 
  anti_join(word.removed, by="word")
tidy.docs$word

tidy.docs <- tidy.docs[-grep("\\d+", tidy.docs$word),]
tidy.docs$word

text.df$text <- gsub("(f|ht)tp\\S+\\s*", "", text.df$text)
text.df$text <- gsub("\\d+", "", text.df$text)
text.df
tidy.docs <- text.df %>% 
  unnest_tokens(output=word, input=text)
tidy.docs$word

stop_words
tidy.docs <- tidy.docs %>% 
  anti_join(stop_words, by="word")
tidy.docs$word

tidy.docs$word <- gsub("\\s+", "", tidy.docs$word)

library(SnowballC)
tidy.docs <- tidy.docs %>% 
  mutate(word=wordStem(word))
tidy.docs$word

tidy.docs$word <- gsub("economist", "economi", tidy.docs$word)
tidy.docs$word

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
library(tm)
corpus.docs <- VCorpus(VectorSource(text))
source <- c("BBC", "CNN", "FOX")
meta(corpus.docs, tag="author", type="local") <- source
lapply(corpus.docs, content)
lapply(corpus.docs, meta)
tidy(corpus.docs)
tidy(corpus.docs) %>% 
  unnest_tokens(word, text) %>% 
  select(source=author, word)

?tidy
methods(tidy)

## 문서-용어행렬

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
library(tm)
corpus.docs <- VCorpus(VectorSource(text))
corpus.docs <- tm_map(corpus.docs, content_transformer(tolower))
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords("english"))
myRemove <- function(x, pattern)
  {return(gsub(pattern, "", x))}
corpus.docs <- tm_map(corpus.docs, content_transformer(myRemove), "(f|ht)tp\\S+\\s*")
corpus.docs <- tm_map(corpus.docs, removePunctuation)
corpus.docs <- tm_map(corpus.docs, removeNumbers)
corpus.docs <- tm_map(corpus.docs, stripWhitespace)
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws))
corpus.docs <- tm_map(corpus.docs, stemDocument)
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub), 
                      pattern="economist", replacement="economi")
corpus.docs
lapply(corpus.docs, content)

corpus.dtm <- DocumentTermMatrix(corpus.docs, control=list(wordLengths=c(2, Inf)))
corpus.dtm
class(corpus.dtm)

nTerms(corpus.dtm)
Terms(corpus.dtm)

nDocs(corpus.dtm)
Docs(corpus.dtm)

rownames(corpus.dtm) <- c("BBC", "CNN", "FOX")
Docs(corpus.dtm)

inspect(corpus.dtm)
inspect(corpus.dtm[1:2, 10:15])

library(tidytext)
tidy(corpus.dtm)

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
source <- c("BBC", "CNN", "FOX")
library(dplyr)
library(tidytext)
library(SnowballC)
text.df <- tibble(source=source, text=text)
text.df$text <- gsub("(f|ht)tp\\S+\\s*", "", text.df$text)
text.df$text <- gsub("\\d+", "", text.df$text)
tidy.docs <- text.df %>% 
  unnest_tokens(output=word, input=text) %>% 
  anti_join(stop_words, by="word")%>% 
  mutate(word=wordStem(word))
tidy.docs$word <- gsub("\\s+", "", tidy.docs$word)
tidy.docs$word <- gsub("economist", "economi", tidy.docs$word)
tidy.docs %>% print(n=Inf)
tidy.docs$word

tidy.docs %>% 
  count(source, word)

tidy.dtm <- tidy.docs %>% 
  count(source, word) %>% 
  cast_dtm(document=source, term=word, value=n)
tidy.dtm
Terms(tidy.dtm)
Docs(tidy.dtm)
inspect(tidy.dtm)
