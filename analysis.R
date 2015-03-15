###----------------------------------------------
# Load libraries and utility functions.
#library("ggplot2")
#source(file.path("R", "XXX.R"))
###----------------------------------------------
# Read the data
f_blogs <- file(file.path("data", "final", "en_US", "en_US.blogs.txt"))
f_news <- file(file.path("data", "final", "en_US", "en_US.news.txt"))
f_twitter <- file(file.path("data", "final", "en_US", "en_US.twitter.txt"))

vc_blogs <- readLines(f_blogs, encoding="UTF-8")  # Ensure encoding is correctly set, else some characters will be gibberish. 
vc_news <- readLines(f_news, encoding="UTF-8") 
vc_twitter <- readLines(f_twitter, encoding="UTF-8") 



