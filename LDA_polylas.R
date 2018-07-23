# Required packages
library(NLP)
library(topicmodels)
library(dplyr)
library(stringi)
library(tm)
library(LDAvis)



####################
## Pre-Processing ##
####################

# Create VCorpus (import directory of txt files with DirSource:)
polylas_il<-VCorpus(DirSource("/Users/sofia/homer_topics_R_2018/Polylas_1890_iliad"), readerControl=list(language="el"))

inspect(polylas_il[[1]])
#inspect(polylas_il[[2]])

#print(polylas_il)
print(polylas_il[[1]])

meta(polylas_il[[1]])

# Structure of R LIST (i.e. VCorpus object)
str(polylas_il)

# VCorpus TRANSFORMATIONS

# Transformation 1: Remove Numbers
polylas_il.trans1<-tm_map(polylas_il, removeNumbers)
inspect(polylas_il.trans1[[1]])

# Transformation 2: Remove Punctuation
polylas_il.trans2<-tm_map(polylas_il.trans1, removePunctuation)
inspect(polylas_il.trans2[[1]])

# Transformation 3: Strip Whitespace
polylas_il.trans3<-tm_map(polylas_il.trans2, stripWhitespace)
inspect(polylas_il.trans3[[1]])


# Create Document Term Matrix ("polylas_il_dtm" from VCorpus "polylas_il.trans3")

#polylas_il_dtm1<-DocumentTermMatrix(polylas_il.trans3)
#inspect(polylas_il_dtm1)

#DTM with minimum word length 5 (instead of stop word removal)
polylas_il_dtm<-DocumentTermMatrix(polylas_il, 
                                control= list(stemming=FALSE, wordLengths = c(5, Inf),
                                              removeNumbers=TRUE, removePunctuation=TRUE))

inspect(polylas_il_dtm)

polylas_il_dtm$v[1:3]
polylas_il_dtm$dimnames$Terms[1:3] #Corpus dictionary: length = 19763



##########################
## Exploring the data!! ##
##########################

findFreqTerms(polylas_il_dtm, 50) #List of words appearing more than 50 times
#wordFreqs_polylas_il below is more informative: gives number of appearances + sorting 


# Dimensions of Document Term Matrices
dim(polylas_il_dtm)
str(polylas_il_dtm)
nrow(polylas_il_dtm) #number of documents (i.e. books of the Iliad)
ncol(polylas_il_dtm) #number of terms
rownames(polylas_il_dtm)
colnames(polylas_il_dtm) #Alternatively: polylas_il$dimnames$Terms


# Dictionary: Example: Document Term Matrix, "polylas_il_dtm", from VCorpus "polylas_il.trans3"
dictionary_polylas_il<-colnames(polylas_il_dtm)
dictionary_polylas_il
length(dictionary_polylas_il)


# CLOUD 1
library(RColorBrewer)
library(wordcloud)

#colSums: frequency of eachy word across all documents,
#then sort cumulative frequency vector

UnsortedFreq<-colSums(as.matrix(polylas_il_dtm))
UnsortedFreq[1:5]
dictionary_polylas_il[1:5]

wordFreqs_polylas_il<-sort(colSums(as.matrix(polylas_il_dtm)), decreasing=TRUE)
wordFreqs_polylas_il[1:1000]
names(wordFreqs_polylas_il)[1:100]
MostFreqNames<-(wordFreqs_polylas_il)[1:100]
MostFreqNames

#hist(wordFreqs_polylas_il)

#wordcloud(words=names(wordFreqs_polylas_il), freq=wordFreqs_polylas_il)
wordcloud(words=names(MostFreqNames), freq=MostFreqNames)

# Create TF-IDF
# Term Frequency-Inverse Document Frequency 
# value increases proportionally to the number of times a word appears in the document,
# BUT it is offset by the frequency of the word in the corpus,
# which helps to adjust for the fact that some words appear more frequently in general.
library(slam)

tfidf_polylas_il<-tapply(polylas_il_dtm$v / row_sums(polylas_il_dtm)[polylas_il_dtm$i], polylas_il_dtm$j, mean) *
  log2(nDocs(polylas_il_dtm)/col_sums(polylas_il_dtm > 0))

is.vector(tfidf_polylas_il) #not a vector
tfidf_polylas_il[1:5]
dictionary_polylas_il[1:5]

length(tfidf_polylas_il)
dim(tfidf_polylas_il)
summary(tfidf_polylas_il)


## Keeping the rows with tfidf >= Median
reduced_dtm_polylas_il<-polylas_il_dtm[,tfidf_polylas_il >= 0.001]

summary(col_sums(reduced_dtm_polylas_il))

inspect(reduced_dtm_polylas_il)

dim(reduced_dtm_polylas_il)

findFreqTerms(reduced_dtm_polylas_il, 5) # at least 5 times

# CLOUD 2
wordFreqs_polylas_il<-sort(colSums(as.matrix(reduced_dtm_polylas_il)), decreasing=TRUE)
wordcloud(words=names(wordFreqs_polylas_il), freq=wordFreqs_polylas_il)



#########
## LDA ##
#########

#Harmonic Mean Function 
#an approximation of p(w|K), i.e., the likelihood of the corpus given the number of topics
#harmonic mean method is used to determine optimal k 
harmonicMean<-function(logLikelihoods, precision = 2000L) {
  llMed<-median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

#Set Topic NUMBER
#Example of the harmonic mean for one value of k, using a burn in of 1000 and iterating 1000 times
k<-25
burnin<-1000
iter<-1000
keep<-50
#log-likelihood values are determined by first fitting the model. 
#This returns all log-likelihood values including burn-in, 
#i.e., these need to be omitted before calculating the harmonic mean:
polylas_il_LDA<-LDA(polylas_il_dtm, k = k, 
                 method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

polylas_il_LDA
str(polylas_il_LDA)

#Assuming that burnin is a multiple of keep; loglik is estimated for part of the data
logLiks_polylas_il<-polylas_il_LDA@logLiks[-c(1:(burnin/keep))]
polylas_il_LDA@logLiks


#To find the optimal value for k for our corpus, 
#we repeat the log-lik, harmonic mean calculations over a sequence of topic models with different vales for k. 
#This will generate numerous topic models with different numbers of topics, creating a vector to hold the k values. 
library(Rmpfr)
library(gmp)

harmonicMean(logLiks_polylas_il)

#Make a sequence of numbers from 2 to 15, stepped by one
seqk<-seq(2, 50, 1) 
seqk
burnin<-1000
iter<-1000
keep<-50
#Run the LDA function using all the values of k, using the lapply function
system.time(fitted_many<-lapply(seqk, function(k) LDA(polylas_il_dtm, k = k,
                                                      method = "Gibbs",control = list(burnin = burnin,
                                                                                      iter = iter, keep = keep) )))

#Extract logliks from each topic
logLiks_many<-lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

#Compute harmonic means
hm_many<-sapply(logLiks_many, function(h) harmonicMean(h))
hm_many

#Optimal number of topics:
#??which.max
which.max(hm_many)
max(hm_many)
seqk
seqk[which.max(hm_many)]


#Optimal LDA plot
library(ggplot2)

plot(seqk, hm_many)

ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  annotate("text", x = 5, y = -10000, 
           label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("polylas_il", atop(italic("How many distinct topics"), ""))))


#Running the model 
# mot_LDA1 from "mot_reduced_dtm" from Document Term Maxtrix "mot_tdm2" from VCorpus "mot_text" 
#(transformations directly applied) with Gibbs sampling 

# mot_LDA2 from "mot_tdm3" from Document Term Maxtrix "mot_tdm3" from VCorpus "mot_text" 
#(no transformations)

#fucntion: LDA(x, k, method = "VEM", control = NULL, model = NULL, ...)

#LDA1 with fixed topics
polylas_il_LDA1 <- LDA(polylas_il_dtm, k = 15, method = "Gibbs", control = list(iter=2000, seed = 0622))

#LDA2 with fixed topics
polylas_il_LDA2 <- LDA(polylas_il_dtm, k = 10, method = "Gibbs", control = list(iter=2000, seed = 0622))


#TOPICS

#Most LIKELY TOPIC for each DOCUMENT
topics1_polylas_il<-topics(polylas_il_LDA2, 2) # 2 = number of topics ranked by likelihood
topics1_polylas_il

#Most frequent TOPIC across DOCUMENTS
most_freq_topic<-which.max(tabulate(topics1_polylas_il))

#Most frequent TERMS per TOPIC
topic_terms<-as.data.frame(terms(polylas_il_LDA2, 10))

terms1<-terms(polylas_il_LDA1, 5)
terms1

terms2<-terms(polylas_il_LDA2, 5)
terms2

##################################################
## POSTERIOR probability of topics per document ## 
##################################################
theta<-as.data.frame(posterior(polylas_il_LDA2)$topics)
theta

topics2<-topics(polylas_il_LDA2, 2)
topics2

topics3<-topics(polylas_il_LDA1, 2)
topics3

junk0 <- as.matrix(polylas_il_dtm)

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

polylas_il_LDA2_json<-topicmodels_json_ldavis(polylas_il_LDA2, polylas_il, polylas_il_dtm)

serVis(polylas_il_LDA2_json)






