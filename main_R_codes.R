# Preload necessary R librabires
library(tm)
library(ggplot2)
library(RWeka)
library(R.utils)
library(dplyr)
library(parallel)
library(wordcloud)

path1 <- "./data/en_US.blogs.txt"
path2 <- "./data/en_US.news.txt"
path3 <- "./data/en_US.twitter.txt"

# Read blogs data in binary mode
conn <- file(path1, open="rb")
blogs <- readLines(conn, skipNul = TRUE, encoding="UTF-8"); close(conn)
# Read news data in binary mode
conn <- file(path2, open="rb")
news <- readLines(conn, skipNul = TRUE, encoding="UTF-8"); close(conn)
# Read twitter data in binary mode
conn <- file(path3, open="rb")
twitter <- readLines(conn, skipNul = TRUE, encoding="UTF-8"); close(conn)
# Remove temporary variable
rm(conn)

set.seed(1011)
portion <- 0.02
sampleBlogs <- blogs[sample(1:length(blogs), portion*length(blogs))]
sampleNews <- news[sample(1:length(news), portion*length(news))]
sampleTwitter <- twitter[sample(1:length(twitter), portion*length(twitter))]

# Remove unconvention/funny characters for sampled Blogs/News/Twitter
# sampleBlogs <- iconv(sampleBlogs, "UTF-8", "ASCII", sub="")
# sampleNews <- iconv(sampleNews, "UTF-8", "ASCII", sub="")
# sampleTwitter <- iconv(sampleTwitter, "UTF-8", "ASCII", sub="")

sampleData <- c(sampleBlogs,sampleNews,sampleTwitter)
writeLines(sampleData, "./samples/sampleData.txt")

remove(blogs,news,twitter,path1,path2,path3)
remove(sampleBlogs,sampleNews,sampleTwitter)

build_corpus <- function (x = sampleData) {
  sample_c <- VCorpus(DirSource("./samples/", encoding = "UTF-8")) # Create corpus dataset
  sample_c <- tm_map(sample_c, content_transformer(tolower)) # all lowercase
  sample_c <- tm_map(sample_c, stripWhitespace) # Strip Whitespace
  sample_c <- tm_map(sample_c, removePunctuation) # Eleminate punctuation
  sample_c <- tm_map(sample_c, removeNumbers) # Eliminate numbers
  # sample_c <- tm_map(sample_c, PlainTextDocument) # Create plain text format
}

corpusData <- build_corpus(sampleData)

# tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
# tdm_1gram <- TermDocumentMatrix(corpusData, control = list(tokenize = tokenizer))
# tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# tdm_2gram <- TermDocumentMatrix(corpusData, control = list(tokenize = tokenizer))
# tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# tdm_3gram <- TermDocumentMatrix(corpusData, control = list(tokenize = tokenizer))
# tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
# tdm_4gram <- TermDocumentMatrix(corpusData, control = list(tokenize = tera.tokenizer))
# remove(tokenizer)

# Define function to make N grams
tdm_Ngram <- function (textcp, n) {
  NgramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))}
  tdm_ngram <- TermDocumentMatrix(textcp, control = list(tokenizer = NgramTokenizer))
  tdm_ngram
}

# Calculate N-Grams
tdm_1gram <- tdm_Ngram(corpusData, 1)
tdm_2gram <- tdm_Ngram(corpusData, 2)
tdm_3gram <- tdm_Ngram(corpusData, 3)
tdm_4gram <- tdm_Ngram(corpusData, 4)

# Define function to extract the N grams and sort
ngram_sorted_df <- function (tdm_ngram) {
  tdm_ngram_m <- as.matrix(tdm_ngram)
  tdm_ngram_df <- as.data.frame(tdm_ngram_m)
  colnames(tdm_ngram_df) <- "Count"
  tdm_ngram_df <- tdm_ngram_df[order(-tdm_ngram_df$Count), , drop = FALSE]
  tdm_ngram_df
}

# Extract term-count tables from N-Grams and sort 
tdm_1gram_df <- ngram_sorted_df(tdm_1gram)
tdm_2gram_df <- ngram_sorted_df(tdm_2gram)
tdm_3gram_df <- ngram_sorted_df(tdm_3gram)
tdm_4gram_df <- ngram_sorted_df(tdm_4gram)

# Save data frames into r-compressed files

quadgram <- data.frame(rows=rownames(tdm_4gram_df),count=tdm_4gram_df$Count)
quadgram$rows <- as.character(quadgram$rows)
quadgram_split <- strsplit(as.character(quadgram$rows),split=" ")
quadgram <- transform(quadgram,first = sapply(quadgram_split,"[[",1),second = sapply(quadgram_split,"[[",2),third = sapply(quadgram_split,"[[",3), fourth = sapply(quadgram_split,"[[",4))
quadgram <- data.frame(unigram = quadgram$first,bigram = quadgram$second, trigram = quadgram$third, quadgram = quadgram$fourth, freq = quadgram$count,stringsAsFactors=FALSE)
write.csv(quadgram[quadgram$freq > 1,],"./ShinyApp/quadgram.csv",row.names=F)
quadgram <- read.csv("./ShinyApp/quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"./ShinyApp/quadgram.RData")


trigram <- data.frame(rows=rownames(tdm_3gram_df),count=tdm_3gram_df$Count)
trigram$rows <- as.character(trigram$rows)
trigram_split <- strsplit(as.character(trigram$rows),split=" ")
trigram <- transform(trigram,first = sapply(trigram_split,"[[",1),second = sapply(trigram_split,"[[",2),third = sapply(trigram_split,"[[",3))
trigram <- data.frame(unigram = trigram$first,bigram = trigram$second, trigram = trigram$third, freq = trigram$count,stringsAsFactors=FALSE)
write.csv(trigram[trigram$freq > 1,],"./ShinyApp/trigram.csv",row.names=F)
trigram <- read.csv("./ShinyApp/trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"./ShinyApp/trigram.RData")


bigram <- data.frame(rows=rownames(tdm_2gram_df),count=tdm_2gram_df$Count)
bigram$rows <- as.character(bigram$rows)
bigram_split <- strsplit(as.character(bigram$rows),split=" ")
bigram <- transform(bigram,first = sapply(bigram_split,"[[",1),second = sapply(bigram_split,"[[",2))
bigram <- data.frame(unigram = bigram$first,bigram = bigram$second,freq = bigram$count,stringsAsFactors=FALSE)
write.csv(bigram[bigram$freq > 1,],"./ShinyApp/bigram.csv",row.names=F)
bigram <- read.csv("./ShinyApp/bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"./ShinyApp/bigram.RData")