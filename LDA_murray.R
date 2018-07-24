# Required packages
library(NLP)
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)
library(RColorBrewer)
library(wordcloud)
library(Rmpfr)
library(gmp)
library(slam)


####################
## Pre-Processing ##
####################

#Create VCorpus (import directory of txt files with DirSource:)
murray_il<-VCorpus(DirSource("/Users/sofia/homer_topics_R_2018/Murray_1924"), readerControl=list(language="en"))

#inspect(murray_il[[1]])
#meta(murray_il[[1]])

#Structure of R LIST (i.e. VCorpus object)
#str(murray_il)

#VCorpus TRANSFORMATIONS

#Transformation 1: Remove Numbers
murray_il.trans1<-tm_map(murray_il, removeNumbers)
#inspect(murray_il.trans1[[1]])

#Transformation 2: Remove Punctuation
murray_il.trans2<-tm_map(murray_il.trans1, removePunctuation)
#inspect(murray_il.trans2[[1]])

#Transformation 3: Strip Whitespace
murray_il.trans3<-tm_map(murray_il.trans2, stripWhitespace)
#inspect(murray_il.trans3[[1]])

# Create Document Term Matrix 

#DTM with minimum word length 5 (instead of stop word removal)
murray_il_dtm<-DocumentTermMatrix(murray_il, 
                                  control= list(stemming=FALSE, wordLengths = c(5, Inf),
                                                removeNumbers=TRUE, removePunctuation=TRUE))

#########
## LDA ##
#########


#LDA1 with fixed topics
#murray_il_LDA1 <- LDA(murray_il_dtm, k = 25, method = "Gibbs", control = list(iter=2000, seed = 0622))

#LDA2 with fixed topics
murray_il_LDA2 <- LDA(murray_il_dtm, k = 10, method = "Gibbs", control = list(iter=2000, seed = 0622))


#TOPICS

#Most LIKELY TOPIC for each DOCUMENT
topics1_murray_il<-topics(murray_il_LDA2, 2) # 2 = number of topics ranked by likelihood
topics1_murray_il

#Most frequent TOPIC across DOCUMENTS
most_freq_topic_murray_il<-which.max(tabulate(topics1_murray_il))

#Most frequent TERMS per TOPIC
topic_terms_murray_il<-as.data.frame(terms(murray_il_LDA2, 10))

#terms1_murray_il<-terms(murray_il_LDA1, 5)
#terms1_murray_il

terms2_murray_il<-terms(murray_il_LDA2, 5)
terms2_murray_il

##################################################
## POSTERIOR probability of topics per document ## 
##################################################
theta_murray_il<-as.data.frame(posterior(murray_il_LDA2)$topics)
theta_murray_il

topics2_murray_il<-topics(murray_il_LDA2, 2)
topics2_murray_il

#LDAvis
#Convert the output of a topicmodels Latent Dirichlet Allocation to JSON
#http://davidmeza1.github.io/2015/07/20/topic-modeling-in-R.html
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
  ## Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- as.matrix(doc_term)
  freq_matrix <- data.frame(ST = colnames(temp_frequency),
                            Freq = colSums(temp_frequency))
  rm(temp_frequency)
  
  ## Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length,
                                 term.frequency = freq_matrix$Freq)
  
  return(json_lda)
}

murray_il_LDA2_json<-topicmodels_json_ldavis(murray_il_LDA2, murray_il, murray_il_dtm)

serVis(murray_il_LDA2_json)
