
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

#######################################
## 제7장 텍스트마이닝 - 7.3 감성분석 ##
#######################################

library(textdata)
lexicon_bing()
lexicon_afinn()
lexicon_nrc()
lexicon_loughran()

library(tidytext)
sentiments
get_sentiments(lexicon="bing")
get_sentiments(lexicon="afinn")
get_sentiments(lexicon="nrc")
get_sentiments(lexicon="loughran")

## 사례: 트윗

library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip"
local.copy <- tempfile()
download.file(url, destfile=local.copy, mode="wb")
Sys.setlocale("LC_TIME", "English")
health.twitter <- 
  map(unzip(zipfile=local.copy, 
            files=c("Health-Tweets/bbchealth.txt",
                    "Health-Tweets/cnnhealth.txt",
                    "Health-Tweets/foxnewshealth.txt",
                    "Health-Tweets/NBChealth.txt")), 
      read_delim, delim="|", quote="", 
      col_types=list(col_character(), col_character(), col_character()),
      col_names=c("id", "datetime", "tweet")) %>% 
  map2(c("bbc", "cnn", "foxnews", "nbc"), ~cbind(.x, source=.y)) %>% 
  reduce(bind_rows) %>% 
  as_tibble() %>% 
  mutate(datetime=ymd_hms(strptime(datetime, "%a %b %d %H:%M:%S +0000 %Y")))
unlink(local.copy)
Sys.setlocale()

health.twitter

health.twitter %>% 
  count(source)

library(tidytext)
library(stringr)
health.words <- health.twitter %>% 
  select(-id) %>%
  mutate(tweet=str_replace_all(tweet, pattern="(f|ht)tp\\S+\\s*", replacement="")) %>%
  mutate(tweet=str_replace_all(tweet, pattern="\\d+", replacement="")) %>%
  mutate(tweet=str_replace_all(tweet, pattern="\\bRT", replacement="")) %>%
  mutate(tweet=str_replace_all(tweet, pattern="@\\S+", replacement="")) %>%
  mutate(tweet=str_replace_all(tweet, pattern="&amp", replacement="")) %>%
  unnest_tokens(word, tweet)
health.words

health.sentiment <- health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>% 
  slice_max(order_by=n, n=10) %>% 
  ungroup() %>% 
  mutate(nsign=ifelse(sentiment=="negative", -n, n))
health.sentiment

# [그림 7-9]
windows(width=7.0, height=5.5)
library(ggplot2)
library(scales)
ggplot(health.sentiment,
       aes(x=reorder(word, nsign), y=nsign,
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_col(color="lightslategray", width=0.8) +
  geom_text(aes(label=n), size=3, color="black",
            hjust=ifelse(health.sentiment$nsign < 0, 1.1, -0.1)) +
  scale_fill_manual(values=c("cornflowerblue", "tomato")) +
  scale_y_continuous(breaks=pretty(health.sentiment$nsign), 
                     labels=abs(pretty(health.sentiment$nsign))) +
  coord_flip() +
  labs(x=NULL, y="Count",
       title="Health News Tweets") +
  theme_minimal() +
  theme(legend.position="bottom", 
        legend.title=element_blank(),
        plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold", size=10))

health.sentiment <- health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word  %in% c("patient", "cancer", "virus"))) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  group_by(sentiment) %>% 
  slice_max(order_by=n, n=10) %>% 
  ungroup() %>% 
  mutate(nsign=ifelse(sentiment=="negative", -n, n))

# [그림 7-10]
windows(width=7.0, height=5.5)
library(ggplot2)
ggplot(health.sentiment,
       aes(x=reorder(word, n), y=n,
           fill=factor(sentiment, levels=c("positive", "negative")))) +
  geom_col(color="lightslategray", width=0.6, show.legend=FALSE) +
  geom_text(aes(label=n), size=3, color="black", hjust=1.2) +
  scale_fill_manual(values=c("lightsteelblue1", "lightsalmon1")) +
  facet_wrap(~ factor(sentiment, levels=c("positive", "negative")), 
             ncol=2, scales="free") +
  coord_flip() +
  labs(x=NULL, y="Count",
       title="Health News Tweets") +
  theme_light() +
  theme(plot.title=element_text(face="bold"),
        axis.line=element_line(color="gray"),
        axis.text=element_text(face="bold", size=10))

# [그림 7-11]
windows(width=5.5, height=5.5)
library(wordcloud)
library(reshape2)
set.seed(123)
health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word  %in% c("patient", "cancer", "virus"))) %>% 
  count(word, sentiment, sort=TRUE) %>% 
  ungroup() %>% 
  acast(formula=word ~ sentiment, value.var="n", fill=0) %>% 
  comparison.cloud(colors=c("tomato", "cornflowerblue"), title.size=2,
                   title.colors=c("red","blue"), title.bg.colors=c("wheat"), 
                   scale=c(4, 0.3), max.words=200, match.colors=TRUE)

health.words
health.sentiment <- health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word  %in% c("patient", "cancer", "virus"))) %>% 
  count(word, sentiment, source, sort=TRUE) %>% 
  ungroup() %>% 
  group_by(source, sentiment) %>% 
  slice_max(order_by=n, n=10) %>% 
  ungroup()
health.sentiment

# [그림 7-12]
windows(width=7.0, height=9.0)
library(ggplot2)
ggplot(health.sentiment, 
       aes(reorder_within(x=word, by=n, within=source), n, fill=source)) + 
  geom_col(show.legend=FALSE) +
  facet_wrap(~ factor(source, 
                      labels=c("BBC", "CNN", "Fox News", "NBC")) + sentiment, 
             ncol=2, scales="free") +
  scale_x_reordered() +
  coord_flip() +
  labs(x=NULL, y="Count",
       title="Health News Tweets") +
  theme_light() +
  theme(strip.background=element_blank(),
        strip.text=element_text(color="goldenrod4", face="bold"),
        plot.title=element_text(face="bold"),
        axis.line=element_line(color="gray"),
        axis.text=element_text(face="bold", size=10),
        panel.grid.minor=element_blank())

health.words
library(lubridate)
health.sentiment <- health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word  %in% c("patient", "cancer", "virus"))) %>% 
  mutate(time=floor_date(x=datetime, unit="month")) %>% 
  count(sentiment, time) %>% 
  group_by(sentiment) %>% 
  slice(2:(n()-1)) %>% 
  ungroup()
health.sentiment

# [그림 7-13]
windows(width=7.0, height=5.5)
Sys.setlocale("LC_TIME", "English")
library(ggplot2)
ggplot(health.sentiment, aes(x=time, y=n, fill=sentiment, color=sentiment)) +
  geom_area(position="identity", alpha=0.3) +
  geom_line(size=1.5) +
  scale_fill_manual(labels=c("Negative", "Positive"), 
                    values=c("orangered", "deepskyblue2")) +
  scale_color_manual(labels=c("Negative", "Positive"), 
                     values=c("orangered", "deepskyblue2")) +
  scale_x_datetime(date_labels="%b %Y", date_breaks="6 months") +
  labs(x=NULL, y="Count",
       title="Health News Tweets") +
  theme_minimal() +
  theme(plot.title=element_text(face="bold"),
        axis.text=element_text(face="bold"),
        legend.position="bottom", 
        legend.title=element_blank())
Sys.setlocale()

# [그림 7-14]
windows(width=9.0, height=9.0)
Sys.setlocale("LC_TIME", "English")
health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word  %in% c("patient", "cancer", "virus"))) %>% 
  mutate(time=floor_date(datetime, unit="month")) %>% 
  count(source, sentiment, time) %>% 
  group_by(source, sentiment) %>% 
  slice(2:(n()-1)) %>% 
  ungroup() %>% 
  ggplot(aes(x=time, y=n, fill=sentiment, color=sentiment)) +
  geom_area(position="identity", alpha=0.3) +
  geom_line(size=1.5) +
  facet_wrap(~ factor(source, 
                      labels=c("BBC", "CNN", "Fox News", "NBC")),
             nrow=4, scales="free") +
  scale_fill_manual(labels=c("Negative", "Positive"), 
                    values=c("coral", "cornflowerblue")) +
  scale_color_manual(labels=c("Negative", "Positive"), 
                     values=c("coral", "cornflowerblue")) +
  scale_x_datetime(date_labels="%b %Y", date_breaks="2 months") +
  labs(x=NULL, y="Count",
       title="Health News Tweets") +
  theme(plot.title=element_text(face="bold"),
        axis.text.x=element_text(size=8),
        legend.position="bottom", 
        legend.title=element_blank())
Sys.setlocale()
