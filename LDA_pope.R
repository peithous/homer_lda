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
pope_il<-VCorpus(DirSource("/Users/sofia/homer_topics_R_2018/Pope_1720"), readerControl=list(language="en"))

#inspect(pope_il[[1]])
#meta(pope_il[[1]])

#Structure of R LIST (i.e. VCorpus object)
#str(pope_il)

#VCorpus TRANSFORMATIONS

#Transformation 1: Remove Numbers
pope_il.trans1<-tm_map(pope_il, removeNumbers)
#inspect(pope_il.trans1[[1]])

#Transformation 2: Remove Punctuation
pope_il.trans2<-tm_map(pope_il.trans1, removePunctuation)
#inspect(pope_il.trans2[[1]])

#Transformation 3: Strip Whitespace
pope_il.trans3<-tm_map(pope_il.trans2, stripWhitespace)
#inspect(pope_il.trans3[[1]])

# Create Document Term Matrix 

#DTM with minimum word length 5 (instead of stop word removal)
pope_il_dtm<-DocumentTermMatrix(pope_il, 
                                  control= list(stemming=FALSE, wordLengths = c(5, Inf),
                                                removeNumbers=TRUE, removePunctuation=TRUE))

#########
## LDA ##
#########


#LDA1 with fixed topics
#pope_il_LDA1 <- LDA(pope_il_dtm, k = 25, method = "Gibbs", control = list(iter=2000, seed = 0622))

#LDA2 with fixed topics
pope_il_LDA2 <- LDA(pope_il_dtm, k = 10, method = "Gibbs", control = list(iter=2000, seed = 0622))


#TOPICS

#Most LIKELY TOPIC for each DOCUMENT
topics1_pope_il<-topics(pope_il_LDA2, 2) # 2 = number of topics ranked by likelihood
topics1_pope_il

#Most frequent TOPIC across DOCUMENTS
most_freq_topic_pope_il<-which.max(tabulate(topics1_pope_il))

#Most frequent TERMS per TOPIC
topic_terms_pope_il<-as.data.frame(terms(pope_il_LDA2, 10))

#terms1_pope_il<-terms(pope_il_LDA1, 5)
#terms1_pope_il

terms2_pope_il<-terms(pope_il_LDA2, 5)
terms2_pope_il

##################################################
## POSTERIOR probability of topics per document ## 
##################################################
theta_pope_il<-as.data.frame(posterior(pope_il_LDA2)$topics)
theta_pope_il

topics2_pope_il<-topics(pope_il_LDA2, 2)
topics2_pope_il

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

pope_il_LDA2_json<-topicmodels_json_ldavis(pope_il_LDA2, pope_il, pope_il_dtm)

serVis(pope_il_LDA2_json)
