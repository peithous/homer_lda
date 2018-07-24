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
pallis_il<-VCorpus(DirSource("/Users/sofia/homer_topics_R_2018/Pallis_1936_missing_bk13"), readerControl=list(language="el"))

inspect(pallis_il[[1]])
meta(pallis_il[[1]])

#Structure of R LIST (i.e. VCorpus object)
str(pallis_il)


#VCorpus TRANSFORMATIONS

#Transformation 1: Remove Numbers
pallis_il.trans1<-tm_map(pallis_il, removeNumbers)
inspect(pallis_il.trans1[[1]])

#Transformation 2: Remove Punctuation
pallis_il.trans2<-tm_map(pallis_il.trans1, removePunctuation)
inspect(pallis_il.trans2[[1]])

#Transformation 3: Strip Whitespace
pallis_il.trans3<-tm_map(pallis_il.trans2, stripWhitespace)
inspect(pallis_il.trans3[[1]])


# Create Document Term Matrix ("pallis_il_dtm" from VCorpus "pallis_il.trans3")

#pallis_il_dtm1<-DocumentTermMatrix(pallis_il.trans3)
#inspect(pallis_il_dtm1)

#DTM with minimum word length 5 (instead of stop word removal)
pallis_il_dtm<-DocumentTermMatrix(pallis_il, 
                                   control= list(stemming=FALSE, wordLengths = c(5, Inf),
                                                 removeNumbers=TRUE, removePunctuation=TRUE))

inspect(pallis_il_dtm)

pallis_il_dtm$v[1:3]
pallis_il_dtm$dimnames$Terms[1:3] #Corpus dictionary: length = ?


#########
## LDA ##
#########


#LDA1 with fixed topics
pallis_il_LDA1 <- LDA(pallis_il_dtm, k = 25, method = "Gibbs", control = list(iter=2000, seed = 0622))

#LDA2 with fixed topics
pallis_il_LDA2 <- LDA(pallis_il_dtm, k = 10, method = "Gibbs", control = list(iter=2000, seed = 0622))


#TOPICS

#Most LIKELY TOPIC for each DOCUMENT
topics1_pallis_il<-topics(pallis_il_LDA2, 2) # 2 = number of topics ranked by likelihood
topics1_pallis_il

#Most frequent TOPIC across DOCUMENTS
most_freq_topic_pallis_il<-which.max(tabulate(topics1_pallis_il))

#Most frequent TERMS per TOPIC
topic_terms_pallis_il<-as.data.frame(terms(pallis_il_LDA2, 10))

terms1_pallis_il<-terms(pallis_il_LDA1, 5)
terms1_pallis_il

terms2_pallis_il<-terms(pallis_il_LDA2, 5)
terms2_pallis_il

##################################################
## POSTERIOR probability of topics per document ## 
##################################################
theta_pallis_il<-as.data.frame(posterior(pallis_il_LDA2)$topics)
theta_pallis_il

topics2_pallis_il<-topics(pallis_il_LDA2, 2)
topics2_pallis_il

topics3_pallis_il<-topics(pallis_il_LDA1, 2)
topics3_pallis_il

junk0 <- as.matrix(pallis_il_dtm)

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

pallis_il_LDA2_json<-topicmodels_json_ldavis(pallis_il_LDA2, pallis_il, pallis_il_dtm)

serVis(pallis_il_LDA2_json)
