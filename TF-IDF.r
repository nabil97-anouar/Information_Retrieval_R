#load required pacakges
if(!require("readtext"))
  install.packages("readtext")
library(readtext)

if(!require("tm"))
  install.packages("tm")
library(tm)

if(!require("stringr"))
  install.packages("stringr")
library(stringr)

if(!require("qdap"))
  install.packages("qdap")
library(qdap)

if(!require("slam"))
  install.packages("slam")
library(slam)

#data files are uploaded at below location:


#load all content files
news_docs = readtext("/Users/mac/Downloads/machinelearning-master/data/*.txt")
news_list = lapply(news_docs[,2],function(x) genX(x, " [", "]"))
N.docs = length(news_list)
names(news_list) = news_docs[,1]

#load search queries
search_queries = readtext("/Users/mac/Downloads/machinelearning-master/query.txt",dvsep = "\n")
queries_list = unlist(strsplit(search_queries[1,2],"\n"))
N.query = length(queries_list)
names(queries_list) = paste0("query", c(1:N.query))

#preprocess data news content
#append both content and search queries together, convert the lists to VectorSource
newscorpus = VectorSource(c(news_list,queries_list))
newscorpus$Names = c(names(news_list),names(queries_list))
#convert to corpus format
newscorpus_preproc = Corpus(newscorpus)
#cleaning the data
newscorpus_preproc = tm_map(newscorpus_preproc,stripWhitespace)
newscorpus_preproc = tm_map(newscorpus_preproc,removePunctuation)
newscorpus_preproc = tm_map(newscorpus_preproc,content_transformer(tolower))
newscorpus_preproc = tm_map(newscorpus_preproc,removeWords,stopwords("english"))


#create tdm using weighted tfidf weightage
tdm = TermDocumentMatrix(newscorpus_preproc,control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
tdm_mat = as.matrix(tdm)
colnames(tdm_mat) = c(names(news_list),names(queries_list))

#normalizing the term document matrix
tfidf_mat <- scale(tdm_mat, center = FALSE,scale = sqrt(colSums(tdm_mat^2)))

#seperating query tdm matrix and content tdm matrix
query.vectors <- tfidf_mat[, (N.docs + 1):(N.docs+N.query)]
tfidf_mat <- tfidf_mat[, 1:N.docs]

#calculating the similarity scores
doc.scores <- t(query.vectors) %*% tfidf_mat

results.df <- data.frame(querylist = queries_list,doc.scores)

#function to display the final results
showTopresults <- function(query){
  x = results.df[which(results.df$querylist == query),]
  yy =  data.frame(t(x),rownames(t(x)),row.names = NULL)[-1,]
  names(yy) = c("score","docs")
  yy$score = as.numeric(as.character(yy$score))
  yyy = yy[order(yy$score,decreasing = T),]
  
  return(yyy[which(yyy$score > 0),][1:3,])
}

#test the function
showTopresults("quantum")
