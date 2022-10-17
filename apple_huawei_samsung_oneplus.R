library(twitteR)    #For fetching tweets
library(tm)         #For VCorpus
library(qdap)       #For Top200words
library(wordcloud)  #For Wordcloud
library(ggplot2)    #For Visualization
library(syuzhet)    #For nrc sentiments

#===============Twitter Authentication==========

key <- "**"
secret <- "***"

token <- "****"
token_s <- "*****"



setup_twitter_oauth(key,secret,token,token_s)


#================================================
#Fetching tweets


a=searchTwitteR("@Apple", 
                  n=500, lang = "en",
                  resultType = "recent")
#View(air)

h=searchTwitteR("@Huawei", 
                  n=500, lang = "en",
                  resultType = "recent")
s=searchTwitteR("@SamsungMobile", 
                   n=500, lang = "en",
                   resultType = "recent")

o=searchTwitteR("@oneplus", 
                   n=500, lang = "en",
                   resultType = "recent")


#Convert tweets to DataFrame
e=twListToDF(a)
f=twListToDF(h)
r=twListToDF(s)
m=twListToDF(o)


#Extract text from Tweets
at=e$text
jt=f$text
vt=r$text
bt=m$text

#==================Text Cleaning==================

# Converting tweets to ASCII to tackle 
#strange characters
at1= iconv(at, from="UTF-8", to="ASCII", sub="")
jt1= iconv(jt, from="UTF-8", to="ASCII", sub="")
vt1= iconv(vt, from="UTF-8", to="ASCII", sub="")
bt1= iconv(bt, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed
at1= gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",at1)
jt1= gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",jt1)
vt1= gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",vt1)
bt1= gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",bt1)

# removing mentions, in case needed
at1= gsub("@\\w+","",at1)
jt1= gsub("@\\w+","",jt1)
vt1= gsub("@\\w+","",vt1)
bt1= gsub("@\\w+","",bt1)

#convert all text to lower case
at1=tolower(at)
jt1=tolower(jt)
vt1=tolower(vt)
bt1=tolower(bt)

# Replace blank space ("rt")
at1=gsub("rt", "", at1)
jt1=gsub("rt", "", jt1)
vt1=gsub("rt", "", vt1)
bt1=gsub("rt", "", bt1)

# Replace @UserName
at1=gsub("@\\w+", "", at1)
jt1=gsub("@\\w+", "", jt1)
vt1=gsub("@\\w+", "", vt1)
bt1=gsub("@\\w+", "", bt1)

# Remove punctuation
at1=gsub("[[:punct:]]", "", at1)
jt1=gsub("[[:punct:]]", "", jt1)
vt1=gsub("[[:punct:]]", "", vt1)
bt1=gsub("[[:punct:]]", "", bt1)

# Remove links
at1=gsub("http\\w+", "", at1)
jt1=gsub("http\\w+", "", jt1)
vt1=gsub("http\\w+", "", vt1)
bt1=gsub("http\\w+", "", bt1)

# Remove tabs
at1=gsub("[ |\t]{2,}", "", at1)
jt1=gsub("[ |\t]{2,}", "", jt1)
vt1=gsub("[ |\t]{2,}", "", vt1)
bt1=gsub("[ |\t]{2,}", "", bt1)

# Remove blank spaces at the beginning
at1=gsub("^ ", "", at1)
jt1=gsub("^ ", "", jt1)
vt1=gsub("^ ", "", vt1)
bt1=gsub("^ ", "", bt1)

# Remove blank spaces at the end
at1=gsub(" $", "", at1)
jt1=gsub(" $", "", jt1)
vt1=gsub(" $", "", vt1)
bt1=gsub(" $", "", bt1)

# Remove numbers
at1=gsub(pattern = "\\d", replacement = " ", at1)
jt1=gsub(pattern = "\\d", replacement = " ", jt1)
vt1=gsub(pattern = "\\d", replacement = " ", vt1)
bt1=gsub(pattern = "\\d", replacement = " ", bt1)

# Remove special characters
at1=gsub(pattern = "\\W", replacement = " ", at1)
jt1=gsub(pattern = "\\W", replacement = " ", jt1)
vt1=gsub(pattern = "\\W", replacement = " ", vt1)
bt1=gsub(pattern = "\\W", replacement = " ", bt1)

# Remove single words
at1=gsub(pattern = "\\b[a-z]\\b{1}", replacement = " ",at1)
jt1=gsub(pattern = "\\b[a-z]\\b{1}", replacement = " ",jt1)
vt1=gsub(pattern = "\\b[a-z]\\b{1}", replacement = " ",vt1)
bt1=gsub(pattern = "\\b[a-z]\\b{1}", replacement = " ",bt1)

#Noting most occuring Frequencies
qdap::freq_terms(at1)
qdap::freq_terms(jt1)
qdap::freq_terms(vt1)
qdap::freq_terms(bt1)

#Corpus Creation
atc=VCorpus(VectorSource(at1))
jtc=VCorpus(VectorSource(jt1))
vtc=VCorpus(VectorSource(vt1))
btc=VCorpus(VectorSource(bt1))

#Removing Stopwords from Corpus 
atc=tm_map(atc, function(x)removeWords(x,stopwords()))
jtc=tm_map(jtc, function(x)removeWords(x,stopwords()))
vtc=tm_map(vtc, function(x)removeWords(x,stopwords()))
btc=tm_map(btc, function(x)removeWords(x,stopwords()))


words=Top200Words
atc=tm_map(atc, function(x)removeWords(x,words))
jtc=tm_map(jtc, function(x)removeWords(x,words))
vtc=tm_map(vtc, function(x)removeWords(x,words))
btc=tm_map(btc, function(x)removeWords(x,words))

atc=tm_map(atc, removeNumbers)
jtc=tm_map(jtc, removeNumbers)
vtc=tm_map(vtc, removeNumbers)
btc=tm_map(btc, removeNumbers)

set.seed(143)

#================================================================

#Visualization

wordcloud(atc,
          min.freq = 5,
          colors=brewer.pal(8, "Dark2"),
          random.order = F,
          scale = c(3,.75),
          rot.per = .4)


wordcloud(jtc,
          min.freq = 5,
          colors=brewer.pal(8, "Dark2"),
          random.order = F,
          scale = c(3,.75),
          rot.per = .4)


wordcloud(vtc,
          min.freq = 5,
          colors=brewer.pal(8, "Dark2"),
          random.order = F,
          scale = c(3,.75),
          rot.per = .4)

wordcloud(btc,
          min.freq = 3,
          colors=brewer.pal(8, "Dark2"),
          random.order = F,
          scale = c(3,.75),
          rot.per = .4)

#============================================
#Sentiment Analysis

#Getting emotions using get nrc sentiment
#Ex sa=Sentiment Airtel

sa=get_nrc_sentiment(at1)
sj=get_nrc_sentiment(jt1)
sv=get_nrc_sentiment(vt1)
sb=get_nrc_sentiment(bt1)


#calculationg total score for each sentiment
#sa_score= Sentiment Airtel Score
sa_score=data.frame(colSums(sa[,]))
sj_score=data.frame(colSums(sj[,]))
sv_score=data.frame(colSums(sv[,]))
sb_score=data.frame(colSums(sb[,]))








sb_score$colSums.sb.....

write.csv(sa, file = 'E:/data-science/GCS training session/gcs project/Apple_sentiment.csv')
write.csv(sj, file = 'E:/data-science/GCS training session/gcs project/Huawei_sentiment.csv')
write.csv(sv, file = 'E:/data-science/GCS training session/gcs project/Samsung_sentiment.csv')
write.csv(sb, file = 'E:/data-science/GCS training session/gcs project/OnePlus_sentiment.csv')


# Clubbing all sentiment scores
Sentiment=data.frame(sa_score$colSums.sa....., 
                     sj_score$colSums.sj.....,
                     sv_score$colSums.sv.....,
                     sb_score$colSums.sb.....)

colnames(Sentiment)=c("Apple","Huawei","Samsung","OnePluse")
rownames(Sentiment)=rownames(sa_score)

st=t(Sentiment)
barplot(st, las=2, 
        beside = T,
        col=c(2,3,4,5),
        ylim=c(0,600),
        legend = TRUE,
        args.legend = list(bty = "n",x = "topleft",ncol = 2))
box()




#========================================================================
# Visualization of Sentiment Analysis by ggplot

#Sentiment Analysis Apple
names(sa_score)<-"Score"
sa_score<-cbind("sentiment"=rownames(sa_score),sa_score)
rownames(sa_score)<-NULL

ggplot(data=sa_score,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments for Apple")

#-------------------------------------
#Sentiment Analysis Plot for huawei 

names(sj_score)<-"Score"
sj_score<-cbind("sentiment"=rownames(sj_score),sj_score)
rownames(sj_score)<-NULL

ggplot(data=sj_score,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+
  ggtitle("Sentiments for Huawei")


#-------------------------------------
#Sentiment Analysis Plot for Samsung

names(sv_score)<-"Score"
sv_score<-cbind("sentiment"=rownames(sv_score),sv_score)
rownames(sv_score)<-NULL

ggplot(data=sv_score,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments for Samsung")

#-------------------------------------

#Sentiment Analysis Plot for One-Plus

names(sb_score)<-"Score"
sb_score<-cbind("sentiment"=rownames(sb_score),sb_score)
rownames(sb_score)<-NULL

ggplot(data=sb_score,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments for Oneplus")

#-------------------------------------
lifecycle::last_lifecycle_warnings()

