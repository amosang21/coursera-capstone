Predictive Text Application for Data Science Specialization Capstone
========================================================
author: Amos Ang
date: 25 Apr 2015


Application Overview
========================================================

- This application allows the user to key in an input phrase, such as "I am", and attempts to predict the next word, based on the input phrase. Generally, the prediction is based on how frequently the words in the input phrase appear in relation to the predicted word (more details in later slide).  

- The application is designed to be able to handle phrases in the English language only. It can also handle **common word contractions** and **profanity elimination**, although the profanity list is just a token one, because we feel that it's rather arbitrary what words are offensive. Nevertheless, this list can be expanded if required. Lastly, hashtags are outright eliminated, as they do not given very good predictive value for the subsequent word.

- Another feature of the application is that it allows the user to control the maximum number of predictions returned by the application. 


Application Algorithm
====
<span style="font-size:18px">
1. **Look-up tables are used**. Given a user input phrase such as "I am", the application will search the look-up table, looking for phrases which start with the input phrase. Once located, these records are returned. The number of records returned depends on the number of predictions desired by the user, as set in the slider control.  
</span>

<span style="font-size:18px">
2. A method colloquially known as "stupid backoff" is used for searching the look-up tables. Basically, the idea is that if the input phrase has 3 words, then the application looks up the entries that have 4 words, with each entry starting with the given input phrase. The entries are ranked in descending order of frequency in which they appear in the sample text ("corpus"). The entries with the highest frequencies are returned first. 
</span>

<span style="font-size:18px">
3. If no entries are found, the application "backs off", and uses the last 2 words from the user input phrase instead. The process is repeated, until some words are predicted. Eventually, some words will have to be predicted, because even if the user inputs nothing, the application will simply look up the most frequently used single words, and return those.


</span>
<!--
2. The look-up tables were pre-created outside of the application itself. The phrases in the look-up tables are obtained based on sampling a combination of text files from blogs, news articles, and twitter tweets, containing a few million lines of text. It is assumed that given the large sample size, the frequencies of the various words and phrases are representative of common usage in the English language.
--> 


Location of Shinyapp and GitHub Repository
========================================================

- Shinyapp: [http://amosang.shinyapps.io/shiny_capstone_project](http://amosang.shinyapps.io/shiny_capstone_project)

- GitHub Repo: [https://github.com/amosang21/coursera-capstone](https://github.com/amosang21/coursera-capstone)


Areas for Further Improvement
========================================================
Due to the limited time available for this project, there are several areas of improvement which can be done, given more resources. These areas are as follows:

1. **Handling phrases which are not present in the look-up tables**. Currently, the application ability to predict is limited to combinations of words which it has seen before. This tends to be phrases/words which appear with high frequency.

2. **Using bigger look-up tables**. Currently, we're constrained by the memory limits on the free tier of shinyapps.io. With more memory, larger look-up tables can be used, and this should conceivably improve the prediction capabilities of the application, as more varieties of multi-word phrases will be present as well. However, we should be careful that the response time of the application does not deteriorate too much.


<!--
- Top Five CSS Customizations for R Presentations http://rstudio-pubs-static.s3.amazonaws.com/27777_55697c3a476640caa0ad2099fe914ae5.html#/1

-->
