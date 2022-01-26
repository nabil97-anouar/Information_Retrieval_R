#Loading of the R packages used in this project
packages <- c("dplyr",
              "magrittr",
              "data.table",
              "tidytext",
              "topicmodels",
              "colorspace",
              "purrr",
              "ldatuning",
              "gmp",
              "wordcloud",
              "RColorBrewer",
              "ggplot2",
              "lubridate","reshape2","textmineR")

#Data Loading from local Machine, Change the directory coressponding to the link of your own dataset
data <- fread("/Users/mac/Downloads/downloadFromKaggle_2-12 (1).csv")


data <- data %>% head(5000)



#Creating the corpus out of our dataset
corpus<-Corpus(VectorSource(data$abstract))

#Removing punctuation
cleancorpus<-tm_map(corpus,content_transformer(removePunctuation))

#Removing Numbers
cleancorpus<-tm_map(cleancorpus,content_transformer(removeNumbers))

#Transforming all letters to lower
cleancorpus<-tm_map(cleancorpus,content_transformer(tolower))
 
#Lemmatization
cleancorpus <- tm_map(cleancorpus, lemmatize_words)

#Removing stopwords in english
cleancorpus<-tm_map(cleancorpus, removeWords, c(stopwords("english"),"can","also","show","two",
                                             "in","one","results","new","first","using")) 




#Creating the DTM
tdm<-DocumentTermMatrix(cleancorpus)


tdm1 <- as.matrix(tdm)

#Extracting the Top terms
frequency <- colSums(tdm1)
frequency <- sort(frequency, decreasing = TRUE)
frequency[1:25]

words <- names(frequency[1:25])

# visualize the top terms as wordcloud
mycolors <- brewer.pal(25, "Dark2")
wordcloud(words, frequency, random.order = FALSE, color = mycolors)



#LDA Parameters 
k = 10 #Number of topics we might find 
seed = 250 #Random number of reproduction.. we set a seed so that the output of the model is predictable
K <- 10
G <- 5000
alpha <- 0.02
eta <- 0.02
docs <- strsplit(data$abstract, split = " ")

# unique words
vocab <- unique( unlist(docs) )

#LDA using the Gibbs Sampling method
lda_fit <- LDA(tdm, k=k, control = list(seed=seed),method = "Gibbs",
               calc_likelihood = TRUE,
               calc_coherence = TRUE,
               calc_r2 = TRUE)

lda_fit@alpha
topics(lda_fit,k)
terms(lda_fit,k)


get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(docs, get.terms)

#Visualisation LDA results
library(ggplot2)
library(tidytext)
library(tidyverse)
topics <- tidy(lda_fit, matrix="beta")

top_terms <- topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)

plot_topic_1 <- top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

plot_topic_1




# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(frequency)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

  
# Find required quantities
phi <- posterior(lda_fit)$terms %>% as.matrix
theta <- posterior(lda_fit)$topics %>% as.matrix
vocab <- colnames(phi)

corp <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

#Creating the Json file in order to visualise the Data
json <- createJSON(phi = corp$phi, 
                   theta = corp$theta, 
                   doc.length = corp$doc.length, 
                   vocab = corp$vocab, 
                   term.frequency = corp$term.frequency)

#Visualising the toplics using LDAvis library
serVis(json, out.dir = 'vis12', open.browser = TRUE)


  
