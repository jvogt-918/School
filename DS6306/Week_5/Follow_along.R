string1 = "Don't let what you can't do interfere with what you can do."
string2 = "\"Don't let what you can't do interfere with what you can do.\" - John Wooden"
writeLines(string2)

string3 = c("IF","YOU","GET","GIVE,","IF","YOU","LEARN","TEACH", "-MAYA","ANGELOU")
string3
str_length(string3)
library(stringr)

str_c(string3, collapse = " ")

string4 = str_c(string3, collapse = " ")
string4
str_sub(string4,1,1)
str_sub(string4,1,7)
str_sub(string4,1,1)
str_sub(string4,-3,-1)
str_sub(string4,-14,-1)
str_sub(string4,3,7)

#Regular Expressions

string5 = " \"I've missed more than 9000 shots in my career. I've lost almost 300 games. 26 times, I've been trusted to take the game winning shot and missed. I've failed over and over and over again in my life. And that is why I succeed.\" - Michael Jordan"
string5

str_view(string5,"shot")
str_view_all(string5,"shot")
str_view_all(string5,"shot.")
str_view_all(string5,"shot\\.")
str_view_all(string5,"\\bshot\\b")
str_view_all(string5, "I")
str_view_all(string5, " I ")
str_view_all(string5, "\\. I")
str_view_all(string5, "(\\. I|\"I)")

#Anchor
string6 = " \"Sometimes when you innovate, you make mistakes. It is best to admit them quickly, and get on with improving your other innovations.\" - Steve Jobs"
str_view(string1,"^Don't")
str_view(string1,"^Interfere")
str_view(string3,"^T")
str_view(string6,"^\"Some")
str_view(string6, "Jobs$")

#Find Digits and whitespace
str_view(string5,"\\d")
str_view(string6,"\\s")
str_view(string6,"^ \"Some")

df = data.frame(quotes = c(string2,string4,string5,string6))
df$quotes
str_view(df$quotes, "(Steve|Jobs)")

df = data.frame(quotes = c(string2,string4,string5,string6), stringsAsFactors = FALSE)
df$quotes

str_view(df$quotes, "[abc]")
str_view(df$quotes, "[^abc]")


#Repititons
# ?: 0 or 1
# +: 1 or more
# *: 0 or more

str_view(df$quotes, "0+")
str_view(df$quotes, "[sn]+")
str_view_all(df$quotes, "[sn]+")


str_view(df$quotes, "[sn]{2}")

#Search for Matches

colors = c("orange","blue","yellow","green","purple","brown","red")
color_expression = str_c("\\b", colors, "\\b")
color_expression = str_c(color_expression, collapse = "|")
color_expression
has_color = str_subset(sentences,color_expression)
has_color
matches = str_extract(has_color,color_expression)
matches
matches_all = str_extract_all(has_color,color_expression, simplify = TRUE)
matches_all
matches_all = unlist(str_extract_all(has_color,color_expression))
matches_all
matchDF = data.frame(Colors = matches_all)
matchDF
matchDF %>% ggplot(aes(x = Colors, fill = Colors)) +
  geom_bar()
matchDF %>% ggplot(aes(x = Colors, fill = Colors)) +
  geom_bar()+
  scale_fill_manual(values=colors)
colors[order(colors)]
matchDF %>% ggplot(aes(x = Colors, fill = Colors)) +
  geom_bar()+
  scale_fill_manual(values=colors[order(colors)])

# Grouped Authors
author = "( -| - )([^ ]{2,}) ([^ ]{2,})"
authors = df$quotes
authors

authors %>% str_extract(author)
authors %>% str_match(author)

dfAuthors = data.frame(authors %>% str_match(author)) 
names(dfAuthors) = c("Full","Dash","First","Last")
dfAuthors
dfAuthors %>% select(c(First,Last))

#NYT 
library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(tidyverse)

#NYTIMES_KEY = "OG89fUubcS8FXofVrLA4dmIOHh5omiFa" #Your Key Here … get from NTY API website
NYTIMES_KEY ="hRa2XPgmxY1gKRtR4iQj8pEwtJ8JGCy2" #Mine Key
term <- "covid" #Need to use + to string together
begin_date <- "20220501"
end_date <- "20220514"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")
baseurl
initialQuery <- jsonlite::fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1)
maxPages

pages <- list()
for(i in 0:maxPages){
  nytSearch <- jsonlite::fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame()
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch
  Sys.sleep(10) #Avoiding HTTP 429 Error
}
nytSearch

allNYTSearch <- rbind_pages(pages)

allNYTSearch %>%
  group_by(response.docs.type_of_material) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=response.docs.type_of_material, fill=response.docs.type_of_material), stat = "identity")

allNYTSearch$NewsOrOther = ifelse(allNYTSearch$response.docs.type_of_material == "News","News","Other")

allNYTSearch[!is.na(allNYTSearch$NewsOrOther),] %>% 
  group_by(NewsOrOther) %>%
  dplyr::summarize(count=n()) %>%
  mutate(percent = (count / sum(count))*100) %>%
  ggplot() +
  geom_bar(aes(y=percent, x=NewsOrOther, fill=NewsOrOther), stat = "identity") + coord_flip()


ArticleToClassify = allNYTSearch[4,] # Check out 4, 9 and 44
ArticleToClassify$response.docs.headline.main

trueType = ArticleToClassify$NewsOrOther[1]
trueType

library(tm) #text mining library provides the stopwords() function
stopwords()

theText = unlist(str_split(str_replace_all(ArticleToClassify$response.docs.headline.main,"[^[:alnum:] ]", ""), boundary("word"))) #Take out all but alpha numeric characters from search string

theText

wordsToTakeOut = stopwords()

#put word boundaries stopwords so that we don't detect partial words later
wordsToTakeOut = str_c(wordsToTakeOut,collapse = "\\b|\\b") 
wordsToTakeOut = str_c("\\b",wordsToTakeOut,"\\b")
wordsToTakeOut

importantWords = theText[!str_detect(theText,regex(wordsToTakeOut,ignore_case = TRUE))]

importantWords

#Find Percentages in News and Other

newsArticles = allNYTSearch %>% filter(NewsOrOther == "News")
otherArticles = allNYTSearch %>% filter(NewsOrOther == "Other")

numNewsArticles = dim(newsArticles)[1]
numOtherArticles = dim(otherArticles)[1]

numNewsArticles
numOtherArticles

thePercentHolderNews = c()
thePercentHolderOther = c()

otherArticles$response.docs.headline.main
importantWords
for(i in 1 : length(importantWords)) #for each important word in the headline
{
  #number of News articles that have the ith word in the headline of interest
  numNews = sum(str_count(newsArticles$response.docs.headline.main,importantWords[i]))
  #number of Other articles that have the ith word in the headline of interest
  numOther = sum(str_count(otherArticles$response.docs.headline.main,importantWords[i]))
  
  #percentage of News articles that have the ith word in the headline of interest 
  thePercentHolderNews[i] = numNews / numNewsArticles
  #percentage of Other articles that have the ith word in the headline of interest
  thePercentHolderOther[i] = numOther / numOtherArticles
  
  #all the News percentages (for each word)
  thePercentHolderNews
  #all the Other percentages (for each word)
  thePercentHolderOther
  
}

thePercentHolderNews
thePercentHolderOther

classifiedAs = if_else(sum(thePercentHolderNews)>sum(thePercentHolderOther),"News","Other")
sum(thePercentHolderNews)
sum(thePercentHolderOther)

Result = str_c("The ", trueType," article was classified as ", classifiedAs, " with a News score of: ",round(sum(thePercentHolderNews),4), " and an Other score of: ", round(sum(thePercentHolderOther),4), ".") 
Result


## VISUALIZE

articleStats = data.frame(Word = importantWords, newsScore = thePercentHolderNews, otherScore = thePercentHolderOther)

# Wide Form / Not Tidy
articleStats

#Tidy and Plot
articleStats[,c(2,3)] %>% gather(Type,Percent) %>% mutate(Word = rep(articleStats$Word,2)) %>% ggplot(aes(y = Percent, x = Type, fill = Word)) + geom_col()

articleStats[,c(2,3)] %>% gather(Type,Percent) %>% mutate(Word = rep(articleStats$Word,2)) %>% ggplot(aes(y = Percent, x = Type, fill = Word)) + geom_col() + facet_wrap(~Word)

articleStats[,c(2,3)] %>% gather(Type,Percent) %>% mutate(Word = rep(articleStats$Word,2)) %>% ggplot(aes(y = Percent, x = Type, fill = Word)) + geom_col() + facet_grid(~Word)
