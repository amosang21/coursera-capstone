# Load libraries and perform other overheads.
library("stylo")
library("tm")
library("data.table")
library("ggplot2")
library("dplyr")

source("R/functions.R")  # Utility functions here.

vc_sample_filenames <- generate_Samples(percent = 0.2)  # Generate sub-samples from the given sample files. File paths to all 3 sub-sample files are returned.

# Generate Freq tables.
dt_ft_1g <- get_FreqTable_FromFiles(vc_sample_filenames, ngram = 1)
dt_ft_2g <- get_FreqTable_FromFiles(vc_sample_filenames, ngram = 2)
dt_ft_3g <- get_FreqTable_FromFiles(vc_sample_filenames, ngram = 3)
dt_ft_4g <- get_FreqTable_FromFiles(vc_sample_filenames, ngram = 4)

# Clean up the freq tables. MAY NOT BE NEEDED. CLEAN UP HAPPENED IN replace_Misc(), even before tokenization.
#dt_ft_1g <- process_FreqTable(dt_ft_1g)

# Persisting key variables to vars.RData.
#save(list = grep("^dt_ft", ls(), value = T), file = "vars_Task3.RData")
rm(list = grep("^dt_ft", ls(), value = T))
#load("vars.RData") 

# Get cum freq table
cft_1 <- get_Cum_Freq(dt_ft_1g)
cft_2 <- get_Cum_Freq(dt_ft_2g)
cft_3 <- get_Cum_Freq(dt_ft_3g)
cft_4 <- get_Cum_Freq(dt_ft_4g)

# Shrink the cum freq tables to appropriate sizes.
dt_cft1 <- shrink_CFT(cft_1, 0.99)  # Consider if need so many entries? We're predicting only 3-5 words!
dt_cft2 <- shrink_CFT(cft_2, 0.90)
dt_cft3 <- shrink_CFT(cft_3, 0.80)
dt_cft4 <- shrink_CFT(cft_4, 0.66)  # Total ~100 Mb. Shrink more? If only 2 cols (ngram+freq), then size of 4 tables is ~76Mb.

# Test with crafted pre-clean pre-tokenized inputs
test4g_1 <- c("to", "take", "a")  # multiple results!
test4g_2 <- c("on", "a", "positive")  # note
test4g_3 <- c("at", "the", "end")  # of
test4g_4 <- c("do", "not", "want")  # to

test3g_1 <- c("I", "do")  # not
test3g_2 <- c("on", "a", "positive")  # note
test3g_3 <- c("at", "the", "end")  # of
test3g_4 <- c("do", "not", "want")  # to


# Basic plot, which shows the number of unique words, against the cum_per_freq of all word instances.
# Shows that a relatively few number of unique words can account for a large number of all word instances, because such words are frequently used.
plot(x = cft_1$per_cum_freq, y = (as.integer(rownames(cft_1))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table", main = "1-gram")
plot(x = cft_2$per_cum_freq, y = (as.integer(rownames(cft_2))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table", main = "2-gram")
plot(x = cft_3$per_cum_freq, y = (as.integer(rownames(cft_3))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table", main = "3-gram")
plot(x = cft_4$per_cum_freq, y = (as.integer(rownames(cft_4))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table", main = "4-gram")
##############

### Tests for removing words from vc_data (corpus)
vi_idx_2char <- grep("^.{2}$", dt_ft_1g$ngram, value = F)
dt_ft_1g_2char <- dt_ft_1g[vi_idx_2char, ]

##############
test <- c("i am Amos", "This is thistletown", "Meow Meow", "am I not handsome?", "is he the one?")
to_remove <- c("am", "is")

remove_Words_From_Vector(to_remove, test)
##############

# Ref: http://www.google.com/support/enterprise/static/postini/docs/admin/en/admin_ee_cu/cm_regex.html
content <- c("rt This is a ReTweet wrtt", "after im done", "go to th")
str <- "(\\b|^)(im|rt|th)(\\b|$)"
gsub(str, "X", content)

##############
for (i in dt_ft_1g_2char$ngram) {
    cat('"', i, '", ', sep = "")
}
##############


