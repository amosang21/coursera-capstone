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
save(list = grep("^dt_ft", ls(), value = T), file = "vars_Task3.RData")
#load("vars.RData") 

# Get cum freq table
cft_1 <- get_Cum_Freq(dt_ft_1g)
cft_2 <- get_Cum_Freq(dt_ft_2g)
cft_3 <- get_Cum_Freq(dt_ft_3g)

# Basic plot, which shows the number of unique words, against the cum_per_freq of all word instances.
# Shows that a relatively few number of unique words can account for a large number of all word instances, because such words are frequently used.
plot(x = cft_1$per_cum_freq, y = (as.integer(rownames(cft_1))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table")
plot(x = cft_2$per_cum_freq, y = (as.integer(rownames(cft_2))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table")
plot(x = cft_3$per_cum_freq, y = (as.integer(rownames(cft_3))), type="l", xaxp = c(0, 1, 100), xlab = "Cumulative Percentage Frequency of All Word Instances", ylab = "Number of Unique Words in Freq Table")

##############

### Tests for removing words from vc_data (corpus)
vi_idx_2char <- grep("^.{2}$", dt_ft_1g$ngram, value = F)
dt_ft_1g_2char <- dt_ft_1g[vi_idx_2char, ]


test <- c("i am Amos", "This is thistletown", "Meow Meow", "am I not handsome?", "is he the one?")
temp <- c("am", "is")

remove_Words_From_Vector(temp, test)

for (i in dt_ft_1g_2char$ngram) {
    cat('"', i, '", ', sep = "")
}
##############



