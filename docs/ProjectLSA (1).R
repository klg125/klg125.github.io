library(dplyr)
library(tidytext)
library(stringr)
library(wordcloud)
library(tidyr)
library(readr)

# tiny example
setwd("/Users/kevinli/Downloads")
corpus = read.csv("recipesamp.csv", sep="|")
corpus
dim(corpus)
names(corpus)

corpus$recipeid_transformed <- rank(corpus$recipeid)
# Scale the rank to be between 1 and 200
corpus$recipeid_transformed <- ceiling((corpus$recipeid_transformed/max(corpus$recipeid_transformed))*200)
corpus$recipeid <- corpus$recipeid_transformed


# convert to tidy data structure
text_df = data_frame(   # text must be of type chr
  id=as.character(corpus$recipeid),
  text = as.character(corpus$recipename))
text_df

recipe_df3 = text_df %>% 
mutate(text = str_to_lower(text)) %>% # convert to lower case
mutate(text = str_replace_all(text, "peanut butter", "peanutbutter")) %>% #replace "peanut butter" with "peanutbutter"
mutate(text = str_replace_all(text, "tex-mex", "texmex")) %>% #replace "tex-mex" with "texmex"
mutate(text = str_replace_all(text, "ice cream", "icecream")) %>% #replace "ice cream" with "icecream"
mutate(text = str_replace_all(text, "cream cheese", "creamcheese")) %>% #replace "cream cheese" with "creamcheese"
mutate(text = str_replace_all(text, "do-ahead", "")) %>% #delete "do-ahead"
mutate(text = str_replace_all(text, "make-ahead", "")) %>% #delete "make-ahead"
mutate(text = str_replace_all(text, "bbq", "barbecue”")) #replace "bbq" with "barbecue"
  
text_df <- recipe_df3

# let's remove stopwords
text_df2 = text_df %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
text_df2

#part c): Bind tf-idf scores to recipe-token pairs 
df2=text_df %>% 
  unnest_tokens(word, text) %>%
  group_by(id) %>%
  count(word, sort=T) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)
df2
df2 %>% arrange(desc(n))
df3 <- left_join(df2, text_df, by = "id")
df3

#part d): LSA 
library(lsa)
dim <- 7
(tiny_dtm =
    cast_dtm(df3, id, word, tf_idf))
fit = lsa(t(tiny_dtm), dims=dim)
#view(round(fit$tk,2))# terms loadings
fit$sk
fit$dk
dk_df <- as.data.frame(fit$dk)

#------------
df <- read_csv("clicksamp.csv")
df$count <- 1
df$recipeid_transformed <- rank(df$recipeid)
df$recipeid_transformed <- ceiling((df$recipeid_transformed/max(df$recipeid_transformed))*200)
df$recipeid <- df$recipeid_transformed
# -------- 
library(dplyr)
dk_df$recipeid <- rownames(dk_df)
df$recipeid <- as.character(df$recipeid)
# Join the df and dk_df dataframes using the recipeid column
joined_df <- left_join(df, dk_df, by = "recipeid")
# Group the joined dataframe by the id column and calculate the mean of the LSA score columns
user_means <- joined_df %>%
  group_by(id) %>%
  summarise(across(starts_with("V"), ~ mean(., na.rm = TRUE)))

# View the resulting dataframe
View(user_means)


#
library(tidyverse)
library(proxy)

# compute distances between users and recipes
dk_df <- dk_df[, c(8, 1:7)]
distances <- as.matrix(dist(rbind(user_means[, -1], dk_df[, -1])))
top_k <- apply(distances[1:nrow(user_means), (nrow(user_means)+1):(nrow(user_means)+nrow(dk_df))], 1, function(x) order(x)[1:10])
# create a data frame of top-k recommendations
top_k_df <- data.frame(id = rep(user_means$id, each = 10),
                       recipeid = dk_df[top_k, 1])
# Add a recommendation number column to top_k_df
top_k_df$recommendation <- rep(1:10, each = nrow(user_means))

# Reshape top_k_df to a wide format (IGNORE)
top_k_df_wide <- top_k_df %>%
  pivot_wider(names_from = recommendation, values_from = recipeid, names_prefix = "recommendation_")
#END OF KEVIN's PART --- 


#part e): 
#Getting errors here, losing dimensionality through sparse matrix
# create lower-dimensional "scores" for each recipe
scores = as.data.frame(fit$dk  %*% fit$sk)
rownames(scores) = df2$id
scores

# create distance matrix using LSA scores
dist_matrix = dist(scores)

# I want you to try some form of CBF. One way to implement CBF would be to take 
#the LSA dimensions from HW5 and compute the mean of the LSAs for each consumer. 
#Then for each user compute the distance from all 50 target items to the user’s 
#LSA mean profile. Select the 10 closest recipes, using say the Euclidean metric. 
#Use distances as with LSA. Try at least one of these.

#Idea:
# use kmeans to cluster
# new mapping table from cluster to recipe

#Find mean profile of LSAs for each customer
# multiply binarized data (0s and 1s for each items) by LSA values for each recipe, then average those
fit$dk["10",]
rb@data@data[,10]

#build out a user profile for each cluster

L = matrix(, nrow = 10000, ncol = 7)
for(i in 1:10000) {
  LSAs <- rep(0, dim)
  for(j in 1:200) {
    if(rb@data@data[j,i]) {
      if(i == 1) {
        print("I: ")
        print(i)
        print("J: ")
        print(j)
        print(fit$dk[toString(j),])
      }
      LSAs <- LSAs + fit$dk[toString(j),]
    }
  }
  #or divide by number opened?
  meanLSA <- LSAs/sum(rb@data@data[,i])
  if(i %% 100 == 0) {
    print(meanLSA)
  }
}

#Find distance from all 50 target items


#Select 10 closest recipes


