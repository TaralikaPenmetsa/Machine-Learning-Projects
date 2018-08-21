##packages required
library(ggplot2)
library(quanteda)
library(RColorBrewer)
library(pROC)

#######Loading the dataset
spam<-read.csv("spam.csv",header=TRUE, sep=",", quote='\"\"', stringsAsFactors=FALSE)

table(spam$v1)

#######plot to check the distribution of type of messages
ggplot(aes(x=v1),data=spam) +
  geom_bar(fill="red",width=0.5)

#######adding appropriate names to the columns of the dataset
names(spam)<-c("type","message")
head(spam)


set.seed(2012)
spam<-spam[sample(nrow(spam)),]

#######constructing a corpus from the Text feilds of our raw data
msg.corpus<-corpus(spam$message)

#######ataching the label to the corpus message text
docvars(msg.corpus)<-spam$type

######buiding spam and ham Wordclouds
#Generating spam wordcloud
#subsetting only the spam messages
spam.plot<-corpus_subset(msg.corpus,docvar1=="spam")

#now creating a document-feature matrix using dfm()
spam.plot<-dfm(spam.plot, tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE)
spam.col <- brewer.pal(10, "BrBG") 
textplot_wordcloud(spam.plot, min_count = 16, color = spam.col)  
title("Spam Wordcloud", col.main = "grey14")

#Generating Ham wordcloud
ham.plot<-corpus_subset(msg.corpus,docvar1=="ham")
ham.plot<-dfm(ham.plot,tolower = TRUE, remove_punct = TRUE, remove_twitter = TRUE, remove_numbers = TRUE,remove=c("gt", "lt"))
ham.col=brewer.pal(10, "BrBG")  
textplot_wordcloud(ham.plot,min.freq=50,colors=ham.col,fixed.asp=TRUE)
title("Ham Wordcloud",col.main = "grey14")

########Predicting using Naive Bayes
#separating Train and test data
spam.train<-spam[1:4458,]
spam.test<-spam[4458:nrow(spam),]

#######generating document freq matrix
msg.dfm <- dfm(msg.corpus, tolower = TRUE)  
msg.dfm <- dfm_trim(msg.dfm, min_count = 5, min_docfreq = 3)  
msg.dfm <- dfm_tfidf(msg.dfm)

#######training and testing data of dfm 
msg.dfm.train<-msg.dfm[1:4458,]

msg.dfm.test<-msg.dfm[4458:nrow(spam),]

#######Training the Naive Bayes classifier-

nb.classifier<-textmodel_nb(msg.dfm.train,spam.train[,1])
nb.classifier

#######Testing the model
pred<-predict(nb.classifier,msg.dfm.test)
pred

#######generating a confusion matrix
table(predicted=pred,actual=spam.test[,1])

#######acccuracy of the classifier on Test data
mean(pred==spam.test[,1])*100

#accuracy on test data is 97%

prednum<-ifelse(pred=="spam",1,2)

auc<-roc(as.factor(spam.test[,1]),prednum)
plot(auc)
auc$auc

#Area under curve is 0.9771
