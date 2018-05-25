# RePublica 2018 in Tweets

library(ggplot2)
library(scales)
library(Cairo)
library(rtweet)
library(xtable)
library(xlsx)
library(tm)

setwd("D:/Stuff/Uni/Weizenbaum/RePublica18/")
load("twittertoken.RDa") 
load("RePublicaTweets.RDa")


##############################
# plot twenty busiest tweeps #
##############################
  # (original code by Felix Haass, tweaked)

df <- data.frame(table(RePublicaTweets$screen_name))
df <- tail(df[order(df$Freq), ], 20)

# get real names
for(i in 1:nrow(df)) {
  df[i, "realname"] <- lookup_users(df[i, "Var1"],  token=twitter_token, tw=F)$name  
}

# create display
df$disp_name <- paste0(df$realname, " \n(@", df$Var1, ")")

# save image
CairoPNG("busiest_rp18_tweeps.png", height=1600, width = 1000, pointsize=30)
par(mar=c(5, 12, 4, 2))
barplot(df$Freq, 
        names.arg=df$disp_name,
        horiz=T, 
        las=1,
        main="20 Busiest #rp18 Tweeps",
        xlab="Tweet count",
        space=0.6, 
        col="navy", 
        border = FALSE, cex.names = .6)

dev.off()


#######################
# Most Popular Tweets #
#######################
# (original code by Felix Haass, tweaked and expanded)

# add popularity count
RePublicaTweets$popularity <- (RePublicaTweets$favorite_count + RePublicaTweets$retweet_count)

# order & subset top 10
ordered <- RePublicaTweets[order(RePublicaTweets$popularity, decreasing = TRUE), ]
top20tweets <- head(ordered[ordered$is_retweet == FALSE, ], 20)

# generate full twitter link
top20tweets$link <- paste0("https://twitter.com/", top20tweets$screen_name, "/status/", top20tweets$status_id)

# write list of tweet links for c & p inclusion to wordpress
write.table(top20tweets[, "link"], row.names=FALSE, file="rp18_twitter.txt", quote=F, col.names=FALSE)

# get real names
for(i in 1:nrow(top20tweets)) {
  top20tweets[i, "realname"] <- lookup_users(top20tweets[i, "screen_name"],  token=twitter_token, tw=F)$name  
}

top20tweets <- subset.data.frame(top20tweets, select=c(created_at, screen_name, text, popularity, link, realname))
write.xlsx(top20tweets,file="top20tweets.xlsx")



###################
# Volume Timeline #
###################
# (original code by Felix Haass, tweaked)

minDate <- min(RePublicaTweets$created_at)
maxDate <- max(RePublicaTweets$created_at)
dateBreaks <- seq(minDate, maxDate, by=60 * 60)
dateBreaks <- c(dateBreaks, maxDate + 60 * 60)
tweetCount <- hist(RePublicaTweets$created_at, breaks=dateBreaks, plot=FALSE)                             
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]

# prepare plot data
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count))

# save plot
CairoPNG("rp18_Twitter_trend.png", width=1600, height=900, pointsize=8, dpi=160)
ggplot(plotData) +
  geom_bar(aes(x=dates, y=tweets), stat="identity") +
  scale_y_continuous("Number of tweets") +
  scale_x_datetime(date_breaks="1 day") +
  theme_bw() +
  theme(axis.text.x=element_text(hjust=1.1, angle=45), legend.key=element_blank())  +
  labs(x="", title="#rp18 Tweets over time \n") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-05-02 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-05-03 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-05-04 00:00:00 UTC"))), color = "firebrick") +
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2018-05-05 00:00:00 UTC"))), color = "firebrick") +
  geom_text(label = "Day 1", aes(x = as.POSIXct("2018-05-02 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Day 2", aes(x = as.POSIXct("2018-05-03 13:00:00 UTC"), y = 300), size = 2.5) +
  geom_text(label = "Day 3", aes(x = as.POSIXct("2018-05-04 13:00:00 UTC"), y = 300), size = 2.5) 

dev.off()

  # Ratelimit der twitter search API wurde mehrfach erreicht - dadurch fehlende Tweets an Tag 1 und 2 !

#######################
# Language processing #
#######################
# (original code by Felix Haass, tweaked)


#### with retweets ####
# get text body
rp_text <- gettext(RePublicaTweets$text)


# clean text

# remove non-ascii characters
rp_text <- gsub("[^\x20-\x7E]", "", rp_text)
# remove html links, h/t http://stackoverflow.com/questions/25352448/remove-urls-from-string-in-r
rp_text <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", rp_text)
# remove @people
rp_text <- gsub("@\\w+", "", rp_text)
# remove punctuation
rp_text <- gsub("[[:punct:]]", " ", rp_text)
# remove numbers
rp_text <- gsub("[[:digit:]]", "", rp_text)
# remove "RT"
rp_text <- gsub("RT", "", rp_text)
# remove "amp" 
rp_text <- gsub(" amp ", "", rp_text)
# remove "http" 
rp_text <- gsub(" http ", "", rp_text)

# create corpus
rp_corpus <- Corpus(VectorSource(rp_text))

# PlainTextDocument Workaround for large Matrix
rp_text <- PlainTextDocument(rp_corpus)

# Term Frequency
term_freq <- termFreq(rp_text, control = list(removePunctuation = TRUE, stopwords = c("#rp18", "rp","#bundeswehr",  "Republica", "Re:Publica", "Re:Publica18", "republica", "Republica18", "RP", "dass", "http", stopwords("english"), stopwords("german")), removeNumbers = TRUE, tolower = TRUE))

dm <- data.frame(word=names(term_freq), freq = term_freq)


words_top <- tail(dm[order(dm$freq), ], 20)

CairoPNG("words_rp18.png", height=1600, width = 1000, pointsize=30)
par(mar=c(5, 8, 4, 2))
barplot(words_top$freq, 
        names.arg=words_top$word,
        horiz=T, 
        las=1,
        main="#rp18 word count",
        xlab="Word count",
        space=0.6, 
        col="navy", 
        border = FALSE, cex.names = .6)

dev.off()

# ber?
ber <- RePublicaTweets[grep("ber", rownames(RePublicaTweets)), ]


# termcorr() function taken from Jay Ulfelder:
# https://github.com/ulfelder/national-security-strategy/blob/master/nss.explore.R

# TDM
rp18_tdm <- TermDocumentMatrix(rp_corpus, control = list(removePunctuation = TRUE, stopwords = c("#rp18", "rp", "Republica", "Re:Publica", "Re:Publica18", "publica", "re", "republica", "Republica18", "RP", "http", stopwords("english"), stopwords("german")), removeNumbers = TRUE, tolower = TRUE))

## Term-Correlation Dotchart

# termcorr <- function(tdm, term, corr = 0.2) {
#  require(Hmisc)

#  z <- findAssocs(tdm, term, corr)
#    dotchart2(z[[1:length(z)]],
#              labels = attr(z$term, "names"),
#              lines = TRUE, lwd = 0.05, lty = 3,
#              sort = FALSE,
#              dotsize = 1.25, pch = 20, col = "firebrick2",
#              cex.labels = 1,
#              xlim = c(0,max(z$term+0.1)))
#    title(main = list(term2, cex = 1.25))
# }
    # Problem mit z$term -> returns NULL


# correlation plot of "Bundeswehr"     funktioniert nicht!
# CairoPNG("termcorr_bundeswehr.png", height=900, width = 1600, pointsize=30)
# termcorr(rp18_tdm,"bundeswehr")
# dev.off()



CairoPNG("termcorr_bundeswehr.png", height=900, width = 1600, pointsize=30)
bundeswehr_assocs <- findAssocs(rp18_tdm, "bundeswehr", 0.15)
bundeswehr_chart <- dotchart2(bundeswehr_assocs[[1:length(bundeswehr_assocs)]],
                      labels = attr(bundeswehr_assocs$bundeswehr, "names"),
                      lines = TRUE, lwd = 0.05, lty = 3,
                      sort = FALSE,
                      dotsize = 1.25, pch = 20, col = "firebrick2",
                      cex.labels = 1,
                      xlim = c(0,max(bundeswehr_assocs$bundeswehr+0.1)))
                    title(main = list("Bundeswehr", cex = 1.25))
dev.off()
    # "straktion" = Störaktion; "aufgeschlsselt" = aufgeschlüsselt



#### without retweets ####

# text body w/o retweets and quotes
rp_text_2 <- subset(RePublicaTweets, subset=(is_retweet==FALSE & is_quote==FALSE))
rp_text_2 <- gettext(rp_text_2$text)

# clean text

# remove non-ascii characters
rp_text_2 <- gsub("[^\x20-\x7E]", "", rp_text_2)
# remove html links, h/t http://stackoverflow.com/questions/25352448/remove-urls-from-string-in-r
rp_text_2 <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", rp_text_2)
# remove @people
rp_text_2 <- gsub("@\\w+", "", rp_text_2)
# remove punctuation
rp_text_2 <- gsub("[[:punct:]]", " ", rp_text_2)
# remove numbers
rp_text_2 <- gsub("[[:digit:]]", "", rp_text_2)
# remove "RT"
rp_text_2 <- gsub("RT", "", rp_text_2)
# remove "amp" 
rp_text_2 <- gsub(" amp ", "", rp_text_2)
# remove "http" 
rp_text_2 <- gsub(" http ", "", rp_text_2)

# create corpus
rp_corpus_2 <- Corpus(VectorSource(rp_text_2))

# PlainTextDocument Workaround for large Matrix
rp_text_2 <- PlainTextDocument(rp_corpus_2)

# Term Frequency
term_freq <- termFreq(rp_text_2, control = list(removePunctuation = TRUE, stopwords = c("#rp18", "rp", "Republica", "Re:Publica", "Re:Publica18", "republica", "Republica18", "RP", "http", stopwords("english"), stopwords("german")), removeNumbers = TRUE, tolower = TRUE))

dm_2 <- data.frame(word=names(term_freq), freq = term_freq)




# termcorr() function taken from Jay Ulfelder:
# https://github.com/ulfelder/national-security-strategy/blob/master/nss.explore.R

# TDM
rp18_tdm_2 <- TermDocumentMatrix(rp_corpus_2, control = list(removePunctuation = TRUE, stopwords = c("#rp18", "rp", "Republica", "Re:Publica", "Re:Publica18", "publica", "re", "republica", "Republica18", "RP", "http", stopwords("english"), stopwords("german")), removeNumbers = TRUE, tolower = TRUE))

## Term-Correlation Dotchart

# termcorr <- function(tdm, term, corr = 0.2) {
#  require(Hmisc)

#  z <- findAssocs(tdm, term, corr)
#    dotchart2(z[[1:length(z)]],
#              labels = attr(z$term, "names"),
#              lines = TRUE, lwd = 0.05, lty = 3,
#              sort = FALSE,
#              dotsize = 1.25, pch = 20, col = "firebrick2",
#              cex.labels = 1,
#              xlim = c(0,max(z$term+0.1)))
#    title(main = list(term2, cex = 1.25))
# }
# Problem mit z$term -> returns NULL


# correlation plot of "Bundeswehr"     funktioniert nicht!
# CairoPNG("termcorr_bundeswehr.png", height=900, width = 1600, pointsize=30)
# termcorr(rp18_tdm,"bundeswehr")
# dev.off()



CairoPNG("termcorr_bundeswehr_noRT.png", height=900, width = 1600, pointsize=30)
bundeswehr_assocs_2 <- findAssocs(rp18_tdm_2, "bundeswehr", 0.15)
bundeswehr_chart_2 <- dotchart2(bundeswehr_assocs_2[[1:length(bundeswehr_assocs_2)]],
                              labels = attr(bundeswehr_assocs_2$bundeswehr, "names"),
                              lines = TRUE, lwd = 0.05, lty = 3,
                              sort = FALSE,
                              dotsize = 1.25, pch = 20, col = "firebrick2",
                              cex.labels = 1,
                              xlim = c(0,max(bundeswehr_assocs_2$bundeswehr+0.1)))
title(main = list("Bundeswehr", cex = 1.25))
dev.off()

############
# Hashtags #
############

hashtag <- table(tolower(unlist(RePublicaTweets$hashtags)))
hashtag <- data.frame(hashtag)
hashtag_top <- hashtag[!(hashtag$Var1=="rp18"),]
hashtag_top <- tail(hashtag_top[order(hashtag_top$Freq), ], 20)
# (#Parlamentsarmee: 6)

# save image
CairoPNG("busiest_rp18_hashtags.png", height=1600, width = 1000, pointsize=30)
par(mar=c(5, 8, 4, 2))
barplot(hashtag_top$Freq, 
        names.arg=hashtag_top$Var1,
        horiz=T, 
        las=1,
        main="20 Busiest #rp18 Hashtags",
        xlab="Tweet count",
        space=0.6, 
        col="navy", 
        border = FALSE, cex.names = .6)

dev.off()
