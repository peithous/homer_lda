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
butler_il<-VCorpus(DirSource("/Users/sofia/homer_topics_R_2018/Butler_1898"), readerControl=list(language="en"))

#inspect(butler_il[[1]])
#meta(butler_il[[1]])

#Structure of R LIST (i.e. VCorpus object)
#str(butler_il)

#VCorpus TRANSFORMATIONS

#Transformation 1: Remove Numbers
butler_il.trans1<-tm_map(butler_il, removeNumbers)
#inspect(butler_il.trans1[[1]])

#Transformation 2: Remove Punctuation
butler_il.trans2<-tm_map(butler_il.trans1, removePunctuation)
#inspect(butler_il.trans2[[1]])

#Transformation 3: Strip Whitespace
butler_il.trans3<-tm_map(butler_il.trans2, stripWhitespace)
#inspect(butler_il.trans3[[1]])

# Create Document Term Matrix 

#DTM with minimum word length 5 (instead of stop word removal)
butler_il_dtm<-DocumentTermMatrix(butler_il, 
                                  control= list(stemming=FALSE, wordLengths = c(5, Inf),
                                                removeNumbers=TRUE, removePunctuation=TRUE))

#########
## LDA ##
#########


#LDA1 with fixed topics
#butler_il_LDA1 <- LDA(butler_il_dtm, k = 25, method = "Gibbs", control = list(iter=2000, seed = 0622))

#LDA2 with fixed topics
butler_il_LDA2 <- LDA(butler_il_dtm, k = 10, method = "Gibbs", control = list(iter=2000, seed = 0622))


#TOPICS

#Most LIKELY TOPIC for each DOCUMENT
topics1_butler_il<-topics(butler_il_LDA2, 2) # 2 = number of topics ranked by likelihood
topics1_butler_il

#Most frequent TOPIC across DOCUMENTS
most_freq_topic_butler_il<-which.max(tabulate(topics1_butler_il))

#Most frequent TERMS per TOPIC
topic_terms_butler_il<-as.data.frame(terms(butler_il_LDA2, 10))

#terms1_butler_il<-terms(butler_il_LDA1, 5)
#terms1_butler_il

terms2_butler_il<-terms(butler_il_LDA2, 5)
terms2_butler_il

##################################################
## POSTERIOR probability of topics per document ## 
##################################################
theta_butler_il<-as.data.frame(posterior(butler_il_LDA2)$topics)
theta_butler_il

topics2_butler_il<-topics(butler_il_LDA2, 2)
topics2_butler_il

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

butler_il_LDA2_json<-topicmodels_json_ldavis(butler_il_LDA2, butler_il, butler_il_dtm)

serVis(butler_il_LDA2_json)

