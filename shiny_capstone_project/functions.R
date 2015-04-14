###### VARIABLES ######
### File pointers (original files) ###
fp_blogs_all <- file.path("data", "final", "en_US", "en_US.blogs.txt")
fp_news_all <- file.path("data", "final", "en_US", "en_US.news.txt")
fp_twitter_all <- file.path("data", "final", "en_US", "en_US.twitter.txt")


###### FUNCTIONS ######

#####
get_Last_Words <- function(vc_input) {
    ### Purpose: Given a char vector with multiple elements, returns the last word of every element, while preserving the original sort order of the elements.
    ### Input: vc_input (char
    ### Output: A character vector containing only the NextWords, in decreasing order of probability.

    ls_words <- strsplit(vc_input, " ")  # Use of strsplit() to break up each element into words.
    vc_output <- NULL  # initialize container variable.
    
    # Get last word for each 
    for (word in ls_words) {
        vc_output <- c(vc_output, tail(word, 1))    
    }
    
    return(vc_output)
}


get_Words_From_Inputs <- function(vc_input_words, dt_cft, num_output_words = 1) {
    ### Purpose: Given a CumFreqTable and InputWords, get the desired number of most likely NextWords. Assumes that the correct N-gram lookup table is given as input!
    ### Input: vc_input_words (tokenized and cleaned user input string); dt_cft (the appropriate cum freq table to use); num_output_words (the number of output words desired)
    ### Output: A character vector containing the NextWords, in decreasing order of probability.
    
    # First, compute num_input_words and num_input_gram
    num_input_words <- length(vc_input_words)  # Get number of words in tokenized user input string.
    if (num_input_words > 3) num_input_words = 3  # Because we cannot handle more than 3 words, as we only have until 4-gram lookup table.
    num_input_gram <- num_input_words + 1  # Always use the n-gram table that is 1 gram more than the word count in the user input string.
    
    vc_input_phrase <- paste("^", paste(vc_input_words, collapse = " "), "\\b", sep = "")  # Create a string from the word tokens, for use with grep in the lookup table.

    vc_output <- grep(vc_input_phrase, dt_cft$ngram, value = T)  # grep the input phrase against the lookup table given.
    num_output <- length(vc_output)
    
    if (num_output_words <= num_output) {
        # Return only the top X records as requested, because there are more records found than requested.
        vc_output <- head(vc_output, num_output_words)
    } else {
        # Return all records found, because what is requested is a greater number of records than what was found.
        
    }
    
    return(get_Last_Words(vc_output))  # Get the last word of every record, and return it.
}


get_Predicted_Words <- function(vc_input, num_output_words = 1) {
    ### Purpose: Given a user input string, returns a number NextWords, subject to a specified maximum number.
    ### Input: vc_input (raw user string input); num_output_words (max number of desired output words).
    ### Output: A character vector containing the NextWords, in decreasing order of probability.

    # Parsing the user's input, to undergo the same cleaning, contraction substitution, profanity elimination, etc, as for the corpus. This will provide some uniformity in the handling.
    vc_input <- iconv(vc_input, from = "UTF-8", to = "ASCII", sub = " ")
    vc_input <- tolower(vc_input)  # Change everything to lowercase, for standarization and easier handling.
    vc_input <- removeNumbers(vc_input)  # Numbers interfere with the text prediction. Remove them for better predictions and easier handling. 
    vc_input <- replace_Contractions(vc_input)  # Replace contractions with their corresponding long forms. Must run BEFORE removePunctuations(), because the latter removes apostrophes!
    vc_input <- removePunctuation(vc_input, preserve_intra_word_dashes = T)  # Punctuation interferes with functionality in gsub() in replace_Misc. Eg: Things like "X)" will not be eliminated.
    vc_input <- remove_Profanities(vc_input)  # Eliminate profanities. If profanities are not here, they will never be suggested. 
    vc_input <- replace_Misc(vc_input)  # Eliminate hashtags, and other arbitrary strings, which are randomly scattered text, and aren't very useful for predictions.
    vc_input <- stripWhitespace(vc_input)  # Eliminate unnecessary whitespaces
    
    # Tokenize the text to words.
    vc_input_words <- txt.to.words(vc_input)  # Need to tokenize to get word count. Alt technique is to use -> sapply(strsplit(str1, " "), length). Note that txt.to.words() does an implicit tolower() already.
    
    num_input_words <- length(vc_input_words)  # Get number of words in tokenized user input string.
    
    if (num_input_words > 3) num_input_words = 3  # Because we cannot handle more than 3 words, as we only have until 4-gram lookup table.
    num_input_gram <- num_input_words + 1  # Always use the n-gram table that is 1 gram more than the word count in the user input string.
    
    vc_input_words <- tail(vc_input_words, num_input_words)  # Take just the last X=num_input_words words.
    
    # Use the 4-gram table.
    if (num_input_gram == 4) {
        vc_output <- get_Words_From_Inputs(vc_input_words, dt_cft4, num_output_words)
        if (length(vc_output) > 0) return (vc_output)
    }
    
    # Use the 3-gram table.
    if (num_input_gram == 3) {
        vc_output <- get_Words_From_Inputs(vc_input_words, dt_cft3, num_output_words)
        if (length(vc_output) > 0) return (vc_output)
    }

    # Use the 2-gram table.
    if (num_input_gram == 2) {
        vc_output <- get_Words_From_Inputs(vc_input_words, dt_cft2, num_output_words)
        if (length(vc_output) > 0) return (vc_output)
    } else {
        # Last fallback position is to simply get X words from the 1-gram table.
        vc_output <- get_Words_From_Inputs("", dt_cft1, num_output_words)  # This works because grep("^\\b") matches every record in the 1-gram table.
        return (vc_output)
    }
}

#####

get_Next_Word <- function(vc_input, n = 1) {
    ### Purpose: Given a string, predict the next word based on the words in the string.
    ### Input: User input string.
    ### Output: data.table of N number of predicted words, ranked in descending order (ie: first row is the most likely one).
    
    # Logically, the user input should be subjected to the same processing logic as the original corpus. For simplification, we make a deliberate decision to be unable to handle/predict profanities or anything that was stripped out.
    vc_input <- iconv(vc_input, from = "UTF-8", to = "ASCII", sub = " ")
    vc_input <- tolower(vc_input)  # Change everything to lowercase, for standarization and easier handling.
    vc_input <- removeNumbers(vc_input)  # Numbers interfere with the text prediction. Remove them for better predictions and easier handling. 
    vc_input <- replace_Contractions(vc_input)  # Replace contractions with their corresponding long forms. Must run BEFORE removePunctuations(), because the latter removes apostrophes!
    vc_input <- removePunctuation(vc_input, preserve_intra_word_dashes = T)  # Punctuation interferes with functionality in gsub() in replace_Misc. Eg: Things like "X)" will not be eliminated.
    vc_input <- remove_Profanities(vc_input)  # Eliminate profanities. If profanities are not here, they will never be suggested. 
    vc_input <- replace_Misc(vc_input)  # Eliminate hashtags, and other arbitrary strings, which are randomly scattered text, and aren't very useful for predictions.
    vc_input <- stripWhitespace(vc_input)  # Don't want whitespace to get in the way.  
    
    return(vc_words_3)
}


shrink_CFT <- function(dt_freqtab, percent_cum_freq) {
    ### Purpose: Given a cumulative freq table, shrink it down to have a coverage of only cum_per_freq (eg: 0.99). 
    ### Input: dt_freqtab (the cumulative freq table).
    ### Output: The reduced dt_freqtab.
    
    dt_freqtab <- filter(dt_freqtab, per_cum_freq <= percent_cum_freq)  # Note: Parameter name cannot be the same as the column name!
    dt_freqtab <- dt_freqtab[ , -c(3:5), with = F]  # Drop columns 3 to 5. Not relevant!
    return(dt_freqtab)
}

### NOT USED ###
process_FreqTable <- function(dt_freqtab) {
    ### Purpose: Given a data.table object (frequency table), clean it by eliminating meaningless words, as best as possible.
    ### Input: dtfreqtab (datatable containing the freq of ngrams)
    ### Output: The cleaned up data.table object.

    # Remove all single-char "words", other than "i", "a".
    dt_freqtab <- dt_freqtab[-grep("^[^ia]$", dt_freqtab$ngram)]  # Notice how the "-grep" takes everything except all single-char "words" (EXCEPT for "i" and "a"!).
    #grep("\\bto\\b|\\bof\\b|\\bis\\b", dt_ft_1g$ngram, value = T)
        
    return(dt_freqtab)
}


get_Cum_Freq <- function(dt_freqtab, cum_percent) {
    ### Purpose: Given a data.table object (frequency table) sorted in descending order of frequency, computes the cumulative frequency and percentage cumulative frequency for each row. If cum_percent is supplied, returns entries only until X% of the cum_percent of word instances.
    ### Input: dtfreqtab (datatable containing the freq of ngrams); cum_percent (cutoff for cumulative percent)
    ### Output: The input data.table object, with added columns of percent_freq, cum_freq, and per_cum_freq.

    # Calculate percentage frequency, for each entry. 
    total <- sum(dt_freqtab$freq)
    dt_freqtab$percent_freq <- dt_freqtab$freq / total

    dt_freqtab$cum_freq <- cumsum(dt_freqtab$freq)  # Cumulative sum.
    dt_freqtab$per_cum_freq <- dt_freqtab$cum_freq / sum(dt_freqtab$freq)    
    
    # Eg: If cum_percent = 0.5, returns only the entries which account collectively for 50% of the word instances.
    if(!missing(cum_percent)) {
        dt_freqtab <- filter(dt_freqtab, per_cum_freq < cum_percent)
    }

    return(dt_freqtab)
}

print_Barchart <- function(dt_freqtab, n, title = "") {
    ### Purpose: Given a data.table object (frequency table), outputs the bar chart for illustration.
    ### Input: dtfreqtab (datatable containing the freq of ngrams); n (prints out only this number of rows in the barchart, if supplied); title(Title of the plot)
    ### Output: A ggplot object, containing the desired bar chart. 
    
    dt_freqtab$ngram <- factor(dt_freqtab$ngram, levels = dt_freqtab$ngram, ordered = T)  # Must be done exactly this way, else ggplot will re-order the x-axis (ahabetically). Ref: http://stackoverflow.com/questions/20041136/how-to-avoid-ggplot-sorting-the-x-axis-while-plotting-geom-bar
    
    # If n is supplied, then just print the first n records in the freq table. Otherwise print all.
    if(!missing(n)) {
        dt_freqtab <- head(dt_freqtab, n)    
    }     
    
    options(scipen = 10)  # So that the y-axis labels in the chart are not in scientific notation.
    g <- ggplot(dt_freqtab) + geom_bar(aes(x = ngram, y = freq), stat = "identity", color = "black", fill = "blue")
    g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, size = 18))
    g <- g + xlab("N-gram") + ylab("Frequency") + ggtitle(title)
    g
}


do_PostProcess_FreqTable <- function(dt_ft) {
    ### Purpose: Given a data.table object, post-process it to reduce the size of the datatable.
    ### Input: A data.table object.
    ### Output: A data.table object.

    # Remove all entries where frequencies == 1. Should eliminate a lot of noise, and also the size of the data table.
    dt_ft <- filter(dt_ft, freq != 1)
    
    return(dt_ft)
}

get_FreqTable_FromFiles <- function(vc_filenames, ngram = 1, nrecs) {
    ### Purpose: Given a character vector of filenames (corpus), combine all files, tokenize them, and return nrecs number of rows of the freq table if specified.
    ### Input: Character vector of filenames; ngram (how to tokenize); nrecs (number of rows of freq table to return)
    ### Output: A data.table object, containing the n-gram and frequency as columns. nrecs is the top number of rows of the freq table.
    
    vc_ngram <- NULL  # Initialize variable.
    # Each file is tokenized, and all the n-grams combined together.
    for (fp_file in vc_filenames) {
        vc_ngram <- c(vc_ngram, tokenize_File(fp_file, ngram = ngram))
    }
    
    # Call to get_FreqTable()
    if (missing(nrecs)) {
        dt_ft_ngram <- get_FreqTable(vc_ngram)  # Ret all rows in the FreqTable.
    } else {
        dt_ft_ngram <- get_FreqTable(vc_ngram, nrecs)
    }
    
    dt_ft_ngram <- do_PostProcess_FreqTable(dt_ft_ngram)  # Try to do post-processing to reduce the freq table size.
    return(dt_ft_ngram)
}


get_FreqTable <- function(vc_tokenized, n = length(vc_tokenized)) {
    ### Purpose: Given a tokenized character vector, return a frequency table of the top N word occurrences, sorted in descending order of frequency. Returns everything if n is not supplied.
    ### Input: Character vector, tokenized into n-grams.
    ### Output: A data.table object, containing the n-gram and frequency as columns.
    t_vc_tokenized <- table(vc_tokenized)
    
    # data.table doesn't have rownames concept, and will drop rownames. Hence must do it this way.     
    dt <- data.table(ngram = rownames(t_vc_tokenized), freq = as.integer(t_vc_tokenized))  # The call to as.integer() is needed because ggplot cannot handle class = table, and will fail.
    dt <- dt[order(-freq)]  # Sort in decreasing order of ngram frequency.
    return(head(dt, n))  
}
    

get_Filestats <- function(fp_name) {
    ### Purpose: Given a file, counts the number of lines and words. Notice the piece-meal technique used to count words, because a work-around for my computer's lack of memory is needed!
    ### Input: File path.
    ### Output: Character vector, with named elements, containing the number of lines and words.
    if (file.exists(fp_name)) {
        con_fp_name <- file(fp_name, encoding = "UTF-8")
        vc_fp_name <- readLines(con_fp_name, skipNul = T)
        vc_fp_name <- iconv(vc_fp_name, from = "UTF-8", to = "ASCII", sub = " ")
        close(con_fp_name)
        
        v_lines <- length(vc_fp_name)  # Number of lines.        
        
        # This technique breaks down the huge char vector into 10 pieces, and passes it piece-by-piece to txt.to.words(), which is unable to handle too large a vector. So we count the number of words in each piece separately, then aggregate them all.
        n <- v_lines / 10
        v_words <- 0                
        for (i in 1:10) {
            v_words <- v_words + length(txt.to.words(vc_fp_name[n*(i-1)+1 : n*i]))
        }        
        
        # Construct and return the character vector.
        vi_return <- c(v_lines, v_words)
        names(vi_return) <- c("num_lines", "num_words")        
        return(vi_return)
        
    } else {
        stop(paste("File does not exist: ", fp_name))
    }    
}

generate_Samples <- function(percent = 0.1) {
    ### Purpose: Convenience function, which generates samples for the 3 data set files used in this project. Returns the names of the paths to all 3 sample files, in case needed (useful).
    ### Input: Percentage of the number of records in each file, to be taken as the sample.
    ### Output: 3 files containing the sample records from each file, are created, in the predefined location.
    
    # Create file pointers.
    n <- as.character(percent * 100)
    
    fp_blogs_all <- file.path("data", "final", "en_US", "en_US.blogs.txt")
    fp_blogs_sample <- file.path("output", paste("en_US.blogs_", n, "percent.txt", sep = ""))
    
    fp_news_all <- file.path("data", "final", "en_US", "en_US.news.txt")
    fp_news_sample <- file.path("output", paste("en_US.news_", n, "percent.txt", sep = ""))
    
    fp_twitter_all <- file.path("data", "final", "en_US", "en_US.twitter.txt")
    fp_twitter_sample <- file.path("output", paste("en_US.twitter_", n, "percent.txt", sep = ""))
        
    # - BLOGS -  
    if (!file.exists(fp_blogs_sample)) {
        # Create file connections.
        con_blogs_all      <- file(fp_blogs_all, encoding = "UTF-8")
        con_blogs_sample   <- file(fp_blogs_sample, encoding = "UTF-8")
        
        # Read in the original file.
        vc_blogs_all <- readLines(con_blogs_all, skipNul = T)
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
        # Create file connections.
        con_news_all       <- file(fp_news_all, encoding = "UTF-8")
        con_news_sample    <- file(fp_news_sample, encoding = "UTF-8")
        
        vc_news_all <- readLines(con_news_all, skipNul = T)
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
        # Create file connections.
        con_twitter_all    <- file(fp_twitter_all, encoding = "UTF-8")
        con_twitter_sample <- file(fp_twitter_sample, encoding = "UTF-8")        
        
        vc_twitter_all <- readLines(con_twitter_all, skipNul = T)
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
    ### Purpose: Given a valid path to a file, reads the contents of the file, processes the contents (cleaning), and tokenizes what remains. N-grams are also created, if specified, otherwise n-gram=1.
    ### Input: Character vector containing a valid path to a file.
    ### Output: Character vector containing the tokenized, processed contents of the file. If specified, the tokens are n-grams; otherwise a default value of ngram=1 is used.
    
    if (!file.exists(filename)) {
        stop("Invalid filename. Unable to open file ", filename)
    } else {
        # Read in the raw lines from the file, using unicode encoding.
        con_file <- file(filename, encoding = "UTF-8")
        vc_file <- readLines(con_file, skipNul = T)
        close(con_file)
        
        # Processing the contents.
        vc_file <- iconv(vc_file, from = "UTF-8", to = "ASCII", sub = " ")
        vc_file <- tolower(vc_file)  # Change everything to lowercase, for standarization and easier handling.
        vc_file <- removeNumbers(vc_file)  # Numbers interfere with the text prediction. Remove them for better predictions and easier handling. 
        vc_file <- replace_Contractions(vc_file)  # Replace contractions with their corresponding long forms. Must run BEFORE removePunctuations(), because the latter removes apostrophes!
        vc_file <- removePunctuation(vc_file, preserve_intra_word_dashes = T)  # Punctuation interferes with functionality in gsub() in replace_Misc. Eg: Things like "X)" will not be eliminated.
        vc_file <- remove_Profanities(vc_file)  # Eliminate profanities. If profanities are not here, they will never be suggested. 
        vc_file <- replace_Misc(vc_file)  # Eliminate hashtags, and other arbitrary strings, which are randomly scattered text, and aren't very useful for predictions.
        vc_file <- stripWhitespace(vc_file)  # Eliminate unnecessary whitespaces

        # Tokenize the text to words.
        vc_words <- txt.to.words(vc_file)  # This call removes apostrophes and other punctuation. Because it's looking for whole words only.        
        # Create ngrams, if parameter is specified. The default parameter of ngram=1 does nothing.
        vc_2gram <- make.ngrams(vc_words, ngram.size = ngram)
    }
}


remove_Words_From_Vector <- function(vc_to_remove, vc_data) {
    ### Purpose: Given a character vector containing a list of words, remove these words from the vc_data object.
    ### Input: vc_to_remove (list of words to remove); vc_data (to remove from here).
    ### Output: The vc_data object with the words removed.
    
    # Makes the words to be removed, case insensitive.
    vc_to_remove <- tolower(vc_to_remove)  
    
    # Craft the regex, and removes every string in the supplied list of words. Note that the regex removes only the exact words (substrings are untouched).
    # Ref: http://www.google.com/support/enterprise/static/postini/docs/admin/en/admin_ee_cu/cm_regex.html
    str <- "(\\b|^)("   
    for(word in vc_to_remove) {        
        str <- paste(str, word, "|", sep = "")
    }
    str <- substr(str, 1, nchar(str) - 1)  # Drop the last character, which contains "|".
    str <- paste(str, ")(\\b|$)", sep = "")    
    vc_data <- gsub(str, " ", vc_data)       

    return(vc_data)
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
    x <- gsub("#\\S+", " ", x)  # \\S+ means "1 or more non-whitespace characters. Ref: http://stackoverflow.com/questions/20310574/regex-replace-all-words-starting-with
    
    # Eliminate all single-char "words", EXCEPT "a" and "i". 
    x <- gsub("\\b[bcdefghjklmnopqrstuvwxyz]\\b", " ", x)
    
    # Eliminate specified 2-char "words". Seem to be mainly short-forms. Not useful for predictions.
    vc_remove <- c("im", "rt", "th", "re", "pm", "ve", "id", "st", "ur", "ya", "ll", "tv", "la", "mr", "ha", "nd", "co", "ah", "vs", "de", "dr", "yo", "dc", "em", "ff", "dm", "rd", "uk", "da", "ny", "fb", "bc", "al", "mt", "ad", "ms", "xd", "aw", "ex", "ed", "cd", "um", "yr", "cc", "ps", "uh", "dj", "hm", "ie", "el", "jr", "ma", "ii", "oo", "hr", "mi", "ca", "pr", "oz", "sf", "un", "mo", "pa", "ft", "ue", "mm", "ty", "xo", "xx", "sc", "ep", "yu", "jk", "tx", "er", "et", "gm", "wi", "af", "np", "ap", "le", "va", "nc", "eu", "bo", "eh", "fl", "ol", "pc", "os", "pp", "sd", "bs", "ye", "en", "ch", "wo", "mp", "nj", "lo", "se", "ga", "hs", "ne", "bf", "lb", "il", "eg", "nw", "dt", "fi", "bb", "cm", "qb", "fa", "na", "cs", "uc", "iv", "wa", "pg", "sb", "ac", "dd", "op", "sa", "ip", "pt", "az", "jo", "kc", "gr", "sm", "ua", "ab", "ho", "ot", "md", "su", "ta", "fm", "tl", "du", "gf", "hd", "ds", "ky", "dh", "ew", "ss", "gb", "rb", "bn", "km", "rm", "mb", "ct", "ml", "ba", "tn", "hp", "bi", "wk", "bp", "td", "sr", "te", "lp", "di", "kg", "nm", "uu", "vp", "mg", "sp", "aa", "mn", "ww", "au", "qa", "dp", "mj", "aj", "uw", "wr", "pi", "tf", "tt", "og", "mc", "fr", "gt", "hw", "ks", "jb", "vi", "bk", "fo", "ub", "rs", "ut", "po", "qr", "lt", "rv", "cf", "mf", "es", "xp", "iu", "js", "mw", "ag", "ar", "ik", "bg", "bt", "cb", "dl", "sq", "ts", "jp", "px", "ui", "ck", "jj", "si", "sw", "cj", "tj", "li", "cp", "fe", "jc", "ux", "ra", "fu", "oc", "pe", "rr", "ai", "sl", "rg", "wu", "bj", "cr", "br", "cu", "av", "ce", "db", "gs", "hq", "ku", "nl", "wv", "ay", "gi", "gd", "nh", "oj", "pb", "bw", "ge", "lg", "nt", "pu", "cv", "ec", "ri", "ht", "qs", "cl", "fx", "ak", "mu", "pd", "hb", "ja", "sh", "ea", "nb", "nf", "pj", "ws", "jt", "kk", "hh", "ia", "ib", "rn", "ti", "bd", "gc", "ls", "ou", "tb", "ul", "ee", "fn", "iq", "nz", "wb", "hv", "nu", "ob", "oy", "pk", "ni", "xi", "fk", "iz", "jd", "jw", "ow", "xs", "ae", "tm", "uf", "bu", "ju", "tu", "bh", "cw", "tr", "uv", "wt", "yg", "eb", "gl", "qt", "zs", "fc", "gp", "kd", "lv", "mk", "cn", "gw", "je", "ko", "ud", "vh", "dw", "nv", "cg", "gg", "pf", "tk", "ao", "dx", "gn", "pl", "rp", "ru", "tc", "vt", "wc", "bv", "ka", "om", "rj", "sj", "xu", "io", "ir", "ji", "lc", "lj", "ph", "rk", "sk", "tp", "wh", "wp", "ax", "hc", "lm", "sg", "yd", "bm", "fs", "ro", "xl", "ze", "ev", "fd", "mh", "oa", "vu", "vw", "wd", "xm", "gh", "jv", "kb", "ki", "vc", "ng", "nn", "rc", "ej", "fg", "fw", "uo", "dg", "fy", "lr", "bl", "fp", "ic", "ln", "lu", "tw", "vo", "dk", "ef", "ek", "hg", "kt", "mv", "ez", "ix", "kl", "od", "ox", "qc", "yw", "gq", "hu", "jm", "jn", "kp", "ov", "vv", "yi", "jl", "lf", "oi", "bx", "ci", "df", "jh", "pw", "cx", "cy", "dq", "ey", "hk", "hn", "ke", "ld", "ly", "ns", "rw", "sn", "tg", "vb", "dv", "gk", "gu", "qu", "vm", "wm", "xc", "yt", "za", "hl", "ig", "kj", "pv", "rl", "ry", "wx", "ei", "hf", "rf", "vg", "eo", "kw", "nr", "wg", "xv", "yk", "cq", "jg", "lw", "qe", "rh", "rx", "vd", "fh", "lk", "oe", "py", "ug", "zz", "dn", "hy", "ij", "mx", "nk", "qi", "sx", "wf", "zo", "eq", "kr", "lh", "vy", "xb", "zi", "fj", "gj", "hj", "jf", "sv", "xy", "ys", "zp", "dy", "gz", "kv", "lq", "qq", "sy", "sz", "vr", "wl", "aq", "bq", "cz", "dz", "fz", "hz", "pq", "tz", "vj", "wj", "xr", "yh", "yj", "yp", "gv", "ih", "iw", "kf", "kh", "kn", "lx", "qo", "vl", "wn", "wy", "fv", "iy", "jq", "jz", "lz", "nq", "nx", "pn", "qd", "qm", "wz", "xj", "xt", "yl", "ym", "yn", "zh", "jy", "kz", "mz", "pz", "qx", "rz", "uy", "uz", "vk", "vn", "xa", "yb", "yc", "yy", "zg", "zn", "zr")
    x <- remove_Words_From_Vector(vc_remove, x)
    
    return(x)
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
