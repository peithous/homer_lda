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
kazatzakis_il<-VCorpus(DirSource("/Users/sofia/homer_topics_R_2018/Kazatzakis"), readerControl=list(language="el"))

inspect(kazatzakis_il[[1]])
meta(kazatzakis_il[[1]])

#Structure of R LIST (i.e. VCorpus object)
str(kazatzakis_il)


#VCorpus TRANSFORMATIONS

#Transformation 1: Remove Numbers
kazatzakis_il.trans1<-tm_map(kazatzakis_il, removeNumbers)
inspect(kazatzakis_il.trans1[[1]])

#Transformation 2: Remove Punctuation
kazatzakis_il.trans2<-tm_map(kazatzakis_il.trans1, removePunctuation)
inspect(kazatzakis_il.trans2[[1]])

#Transformation 3: Strip Whitespace
kazatzakis_il.trans3<-tm_map(kazatzakis_il.trans2, stripWhitespace)
inspect(kazatzakis_il.trans3[[1]])


# Create Document Term Matrix ("kazatzakis_il_dtm" from VCorpus "kazatzakis_il.trans3")

#kazatzakis_il_dtm1<-DocumentTermMatrix(kazatzakis_il.trans3)
#inspect(kazatzakis_il_dtm1)

#DTM with minimum word length 5 (instead of stop word removal)
kazatzakis_il_dtm<-DocumentTermMatrix(kazatzakis_il, 
                                  control= list(stemming=FALSE, wordLengths = c(5, Inf),
                                                removeNumbers=TRUE, removePunctuation=TRUE))

inspect(kazatzakis_il_dtm)

kazatzakis_il_dtm$v[1:3]
kazatzakis_il_dtm$dimnames$Terms[1:3] #Corpus dictionary: length = ?


#########
## LDA ##
#########


#LDA1 with fixed topics
kazatzakis_il_LDA1 <- LDA(kazatzakis_il_dtm, k = 25, method = "Gibbs", control = list(iter=2000, seed = 0622))

#LDA2 with fixed topics
kazatzakis_il_LDA2 <- LDA(kazatzakis_il_dtm, k = 10, method = "Gibbs", control = list(iter=2000, seed = 0622))


#TOPICS

#Most LIKELY TOPIC for each DOCUMENT
topics1_kazatzakis_il<-topics(kazatzakis_il_LDA2, 2) # 2 = number of topics ranked by likelihood
topics1_kazatzakis_il

#Most frequent TOPIC across DOCUMENTS
most_freq_topic_kazatzakis_il<-which.max(tabulate(topics1_kazatzakis_il))

#Most frequent TERMS per TOPIC
topic_terms_kazatzakis_il<-as.data.frame(terms(kazatzakis_il_LDA2, 10))

terms1_kazatzakis_il<-terms(kazatzakis_il_LDA1, 5)
terms1_kazatzakis_il

terms2_kazatzakis_il<-terms(kazatzakis_il_LDA2, 5)
terms2_kazatzakis_il

##################################################
## POSTERIOR probability of topics per document ## 
##################################################
theta_kazatzakis_il<-as.data.frame(posterior(kazatzakis_il_LDA2)$topics)
theta_kazatzakis_il

topics2_kazatzakis_il<-topics(kazatzakis_il_LDA2, 2)
topics2_kazatzakis_il

topics3_kazatzakis_il<-topics(kazatzakis_il_LDA1, 2)
topics3_kazatzakis_il

junk0 <- as.matrix(kazatzakis_il_dtm)

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

kazatzakis_il_LDA2_json<-topicmodels_json_ldavis(kazatzakis_il_LDA2, kazatzakis_il, kazatzakis_il_dtm)

serVis(kazatzakis_il_LDA2_json)

