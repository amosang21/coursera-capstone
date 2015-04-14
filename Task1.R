# Create file pointers.
fp_blogs_all <- file.path("data", "final", "en_US", "en_US.blogs.txt")
fp_blogs_sample <- file.path("output", "en_US.blogs_sample.txt")

fp_news_all <- file.path("data", "final", "en_US", "en_US.news.txt")
fp_news_sample <- file.path("output", "en_US.news_sample.txt")

fp_twitter_all <- file.path("data", "final", "en_US", "en_US.twitter.txt")
fp_twitter_sample <- file.path("output", "en_US.twitter_sample.txt")

# Create file connections.

con_twitter <- file(fp_twitter_all, encoding = "UTF-8")
con_twitter_sample <- file(fp_twitter_sample, encoding = "UTF-8")

if (!file.exists(fp_twitter_sample)) {
    # Read in the original twitter file
    vc_twitter_all <- readLines(con_twitter)    
    
    # Do a random sample of the rows. Take 10% of the total entries (~230K tweets)
    vc_twitter_sample <- sample(vc_twitter_all,size = length(vc_twitter_all) * 0.1)
    # Output the twitter sub-sample file.    
    write(vc_twitter_sample, file = con_twitter_sample)
    
} else {  # Sample file already exists, so read this instead.
    vc_twitter_sample <- readLines(con_twitter_sample)    
}

# Close off the connections
close(con_twitter)
close(con_twitter_sample)

# Cleaning up
rm(vc_twitter_all)  # Variable will not exist, if we read vc_twitter_sample directly.

# Tokenize vc_twitter_sample
library("stylo")
library("tm")
library("data.table")

x <- head(vc_twitter_sample, 200000)  # Tiny sample. For testing how tokenization works

x <- iconv(x, from="UTF-8", to="latin1", sub=" ")
x <- tolower(x)  # Change everything to lowercase, for standarization and easier handling.
x <- removeNumbers(x)  # Numbers interfere with the text prediction. Remove them for better predictions and easier handling. 
x <- remove_Profanities(x)  # Eliminate profanities. If profanities are not here, they will never be suggested. 
x <- replace_Misc(x)  # Eliminate hashtags, which are randomly scattered text, and aren't very useful for predictions.
x <- replace_Contractions(x)  # Replace contractions with their corresponding long forms.
x <- stripWhitespace(x)  # Eliminate unnecessary whitespaces

vc_words <- txt.to.words(x)  # This call removes apostrophes and other punctuation. Because it's looking for whole words only.
vc_2gram <- make.ngrams(vc_words, ngram.size = 2)
dt <- as.data.table(table(vc_2gram))


###### UTILITY FUNCTIONS #####

generate_Samples <- function(percent = 0.1) {
    ### Purpose: Convenience function, which generates samples for the 3 data set files used in this project. Returns the names of the paths to all 3 sample files, in case needed (useful).
    ### Input: Percentage of the number of records in each file, to be taken as the sample.
    ### Output: 3 files containing the sample records from each file, are created, in the predefined location.

    # Create file pointers.
    fp_blogs_all <- file.path("data", "final", "en_US", "en_US.blogs.txt")
    fp_blogs_sample <- file.path("output", "en_US.blogs_sample.txt")
    
    fp_news_all <- file.path("data", "final", "en_US", "en_US.news.txt")
    fp_news_sample <- file.path("output", "en_US.news_sample.txt")
    
    fp_twitter_all <- file.path("data", "final", "en_US", "en_US.twitter.txt")
    fp_twitter_sample <- file.path("output", "en_US.twitter_sample.txt")
    
    # Create file connections.
    con_blogs_all      <- file(fp_blogs_all, encoding = "UTF-8")
    con_news_all       <- file(fp_news_all, encoding = "UTF-8")
    con_twitter_all    <- file(fp_twitter_all, encoding = "UTF-8")
    con_blogs_sample   <- file(fp_blogs_sample, encoding = "UTF-8")
    con_news_sample    <- file(fp_news_sample, encoding = "UTF-8")
    con_twitter_sample <- file(fp_twitter_sample, encoding = "UTF-8")
    
    # - BLOGS -  
    if (!file.exists(fp_blogs_sample)) {
        # Read in the original file.
        vc_blogs_all <- readLines(con_blogs_all)
        # Do a random sample of the rows. Take 10% of the total entries.    
        vc_blogs_sample <- sample(vc_blogs_all, size = length(vc_blogs_all) * percent)
        rm(vc_blogs_all)
        # Output the sub-sample file.    
        write(vc_blogs_sample, file = con_blogs_sample)
        rm(vc_blogs_sample)
        close(con_blogs_all); close(con_blogs_sample)
    } else {
        print(paste("File already exists. Did not regenerate.", fp_blogs_sample))
    }
    
    # - NEWS -     
    if (!file.exists(fp_news_sample)) {
        vc_news_all <- readLines(con_news_all)
        vc_news_sample <- sample(vc_news_all, size = length(vc_news_all) * percent)
        rm(vc_news_all)
        write(vc_news_sample, file = con_news_sample)
        rm(vc_news_sample)
        close(con_news_all); close(con_news_sample)
    } else {
        print(paste("File already exists. Did not regenerate.", fp_news_sample))
    }
    
    # - TWITTER -    
    if (!file.exists(fp_twitter_sample)) {    
        vc_twitter_all <- readLines(con_twitter_all)
        vc_twitter_sample <- sample(vc_twitter_all, size = length(vc_twitter_all) * percent)
        rm(vc_twitter_all)
        write(vc_twitter_sample, file = con_twitter_sample)
        rm(vc_twitter_sample)
        close(con_twitter_all); close(con_twitter_sample) 
    } else {
        print(paste("File already exists. Did not regenerate.", fp_twitter_sample))
    }

    # Return the names of the paths to all 3 sample files, in case needed. Useful.
    return(c(fp_blogs_sample, fp_news_sample, fp_twitter_sample))
}


tokenize_File <- function(filename, ngram = 1) {    
    ### Purpose: Removes certain profanities, substituting them with nothing. Just some token words, to demonstrate that this is possible. 
    ### Input: Character vector containing a valid path to a file.
    ### Output: Character vector containing the tokenized, processed contents of the file. If specified, the tokens are n-grams; otherwise a default value of ngram=1 is used.

    if (!file.exists(filename)) {
        stop("Invalid filename. Unable to open file ", filename)
    } else {
        # Read in the raw lines from the file, using unicode encoding.
        con_file <- file(filename, encoding = "UTF-8")
        vc_file <- readLines(con_file)
        close(con_file)
        
        # Processing the contents.
        vc_file <- iconv(vc_file, from="UTF-8", to="latin1", sub=" ")
        vc_file <- tolower(vc_file)  # Change everything to lowercase, for standarization and easier handling.
        vc_file <- removeNumbers(vc_file)  # Numbers interfere with the text prediction. Remove them for better predictions and easier handling. 
        vc_file <- remove_Profanities(vc_file)  # Eliminate profanities. If profanities are not here, they will never be suggested. 
        vc_file <- replace_Misc(vc_file)  # Eliminate hashtags, which are randomly scattered text, and aren't very useful for predictions.
        vc_file <- replace_Contractions(vc_file)  # Replace contractions with their corresponding long forms.
        vc_file <- stripWhitespace(vc_file)  # Eliminate unnecessary whitespaces
        
        # Tokenize the text to words.
        vc_words <- txt.to.words(vc_file)  # This call removes apostrophes and other punctuation. Because it's looking for whole words only.        
        # Create ngrams, if parameter is specified. The default parameter of ngram=1 does nothing.
        vc_2gram <- make.ngrams(vc_words, ngram.size = ngram)
    }
}


remove_Profanities <- function(x) {
    ### Purpose: Removes certain profanities, substituting them with nothing. Just some token words, to demonstrate that this is possible. 
    ### Input: Character vector.
    ### Output: Character vector.     
    
    x <- gsub("\\S*[Ff][Uu][Cc][Kk]\\S*|\\S*[Cc][Uu][Nn][Tt]\\S*", "", x)  # \\S* means "0 or more non-whitespace characters. 
}

replace_Misc <- function(x) {
    ### Purpose: General function for replacing anything else.
    ### Input: Character vector.
    ### Output: Character vector.     
    
    # Eliminate hashtags.
    x <- gsub("#\\S+", "", x)  # \\S+ means "1 or more non-whitespace characters. Ref: http://stackoverflow.com/questions/20310574/regex-replace-all-words-starting-with
}

replace_Contractions <- function(x) {
    ### Purpose: Transform common English contractions, using gsub().
    ### Input: Character vector.
    ### Output: Character vector.     
    
    # "'d" -> " would". 
    x <- gsub("'d", " would", x)    
    #     x <- gsub("he'd", "he would", x)
    #     x <- gsub("how'd", "how would", x) 
    #     x <- gsub("i'd", "i would", x)
    #     x <- gsub("it'd", "it would", x)
    #     x <- gsub("she'd", "she would", x)
    #     x <- gsub("there'd", "there would", x)
    #     x <- gsub("they'd", "they would", x)
    #     x <- gsub("we'd", "we would", x)
    #     x <- gsub("where'd", "where would", x)
    #     x <- gsub("who'd", "who would", x)
    #     x <- gsub("you'd", "you would", x)    
    
    # "'re" -> " are". 
    x <- gsub("'re", " are", x)
    #     x <- gsub("there're", "there are", x)
    #     x <- gsub("they're", "they are", x)  
    #     x <- gsub("we're", "we are", x)
    #     x <- gsub("what're", "what are", x)
    #     x <- gsub("who're", "who are", x)
    #     x <- gsub("why're", "why are", x)
    #     x <- gsub("you're", "you are", x)     
    
    # "n't" -> " not". 
    ##- Handle exceptions to the rule first.
    x <- gsub("can't", "cannot", x)
    x <- gsub("won't", "will not", x)
    x <- gsub("ain't", "am not", x)
    x <- gsub("shan't", "shall not", x)
    
    x <- gsub("n't", " not", x)
    #     x <- gsub("aren't", "are not", x)
    #     x <- gsub("can't", "cannot", x)
    #     x <- gsub("couldn't", "could not", x)
    #     x <- gsub("didn't", "did not", x)
    #     x <- gsub("doesn't", "does not", x)
    #     x <- gsub("don't", "do not", x)
    #     x <- gsub("hadn't", "had not", x)
    #     x <- gsub("hasn't", "has not", x)
    #     x <- gsub("haven't", "have not", x)
    #     x <- gsub("isn't", "is not", x)
    #     x <- gsub("mightn't", "might not", x)
    #     x <- gsub("mustn't", "must not", x)
    #     x <- gsub("needn't", "need not", x)
    #     x <- gsub("oughtn't", "ought not", x)
    #     x <- gsub("shouldn't", "should not", x)
    #     x <- gsub("wasn't", "was not", x)
    #     x <- gsub("weren't", "were not", x)
    #     x <- gsub("wouldn't", "would not", x)
    
    
    # "'s" -> " is". Note: possession is not handled (eg: granny's)
    ##- Handle exceptions to the rule first.
    x <- gsub("let's", "let us", x)
    
    x <- gsub("'s", " is", x)
    #     x <- gsub("he's", "he is", x)
    #     x <- gsub("how's", "how is", x)
    #     x <- gsub("it's", "it is", x)
    #     x <- gsub("she's", "she is", x)
    #     x <- gsub("that's", "that is", x)
    #     x <- gsub("there's", "there is", x)
    #     x <- gsub("what's", "what is", x)
    #     x <- gsub("when's", "when is", x)
    #     x <- gsub("where's", "where is", x)
    #     x <- gsub("who's", "who is", x)
    #     x <- gsub("why's", "why is", x)
    
    # "'ve" -> " have"
    x <- gsub("'ve", " have", x)    
    #     x <- gsub("could've", "could have", x)
    #     x <- gsub("i've", "i have", x)
    #     x <- gsub("might've", "might have", x)
    #     x <- gsub("must've", "must have", x)
    #     x <- gsub("should've", "should have", x)
    #     x <- gsub("they've", "they have", x)
    #     x <- gsub("we've", "we have", x)
    #     x <- gsub("what've", "what have", x)
    #     x <- gsub("who've", "who have", x)
    #     x <- gsub("would've", "would have", x)
    #     x <- gsub("you've", "you have", x)    
    
    # "'ll" -> " will"
    x <- gsub("'ll", " will", x)    
    #     x <- gsub("he'll", "he will", x)
    #     x <- gsub("how'll", "how will", x)
    #     x <- gsub("i'll", "i will", x)
    #     x <- gsub("it'll", "it will", x)
    #     x <- gsub("she'll", "she will", x)
    #     x <- gsub("that'll", "that will", x)
    #     x <- gsub("they'll", "they will", x)
    #     x <- gsub("we'll", "we will", x)
    #     x <- gsub("what'll", "what will", x)
    #     x <- gsub("who'll", "who will", x)
    #     x <- gsub("why'll", "why will", x)
    #     x <- gsub("you'll", "you will", x)
    
    
    # Misc
    x <- gsub("i'm", "i am", x)
    x <- gsub("ma'am", "madam", x)  
    x <- gsub("y'all", "you all", x)
    #x <- gsub("couldn't've", "could not have", x)
    #x <- gsub("hadn't've", "had not have", x)    
    #x <- gsub("i'd've", "i would have", x)
    #x <- gsub("it'd've", "it would have", x)
    #x <- gsub("mightn't've", "might not have", x)
    #x <- gsub("not've", "not have", x)
    #x <- gsub("o'clock", "of the clock", x)
    #x <- gsub("she'd've", "she would have", x)
    #x <- gsub("shouldn't've", "should not have", x)
    #x <- gsub("there'd've", "there would have", x)
    #x <- gsub("they'd've", "they would have", x)
    #x <- gsub("we'd've", "we would have", x)
    #x <- gsub("where've", "where have", x)
    #x <- gsub("who'd've", "who would have", x)
    #x <- gsub("wouldn't've", "would not have", x)    
    #x <- gsub("you'd've", "you would have", x)    
}
