###----------------------------------------------
# Load libraries and utility functions.
library("tm")
library("RWeka")
#source(file.path("R", "XXX.R"))
###----------------------------------------------
# Read the data
f_blogs <- file(file.path("data", "final", "en_US", "en_US.blogs.txt"))
f_news <- file(file.path("data", "final", "en_US", "en_US.news.txt"))
f_twitter <- file(file.path("data", "final", "en_US", "en_US.twitter.txt"))

vc_blogs <- readLines(f_blogs, encoding="UTF-8")  # Ensure encoding is correctly set, else some characters will be gibberish. 
vc_news <- readLines(f_news, encoding="UTF-8") 
vc_twitter <- readLines(f_twitter, encoding="UTF-8") 

# Merge all 3 files into 1 combined character vector.
vc_combined <- c(vc_blogs, vc_news, vc_twitter)
rm(list = c("vc_blogs", "vc_news", "vc_twitter"))


doc.vec <- VectorSource(vc_combined)  # Creates a vector source.
doc.corpus <- Corpus(doc.vec)

doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))  # http://stackoverflow.com/questions/25551514/termdocumentmatrix-errors-in-r
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))  # http://en.wikipedia.org/wiki/Stop_words
#doc.corpus <- tm_map(doc.corpus, stemDocument)  # MAY NOT NEED TO DO THIS. Uses Porter's stemming algorithm in SnowballC package.
doc.corpus <- tm_map(doc.corpus, stripWhitespace)  # characters like <U+0093>  still present!


###-------TEST: Tokenizer functinality in RWeka
library("RWeka")
library("tm")

data("crude")

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))

inspect(tdm[340:345,1:10])

###-------TEST STYLO package
library("stylo")

# Test on the smallest file first.
corpus <- load.corpus(corpus.dir = file.path("data", "final", "en_US"),
                      files      = list.files(pattern="en_US.news.txt"),
                      #files      = list.files(pattern="[.]txt$"),
                      encoding   = "UTF-8")

corpus[[1]][1:10]

t <- txt.to.words(corpus)


token_corpus <- txt.to.words()

###----------------------------------------------