project_org_csv<-read.csv(choose.files())
summary(project_org_csv$Class)
sample_mail<-read.csv(choose.files())
sample_mail$filename<-as.character(sample_mail$filename)
split<-strsplit(sample_mail$filename,"/")
#strsplit("arnold-j/_sent_mail/34.", "/")
sample_mail$name<-NA
sample_mail$file_type<-NA
sample_mail$file_number<-NA
sample_mail$word_count<-NA
sample_mail$line_count<-NA
sample_mail$corpus_Word_count<-NA
split<-as.list(split)
for (i in 1:2221) {
    sample_mail$name[i]<-split[[i]][1]
    sample_mail$file_type[i]<-paste(as.character(c(split[[i]][-c(1,length(split[[i]]))])),collapse = " ")

    sample_mail$file_number[i]<-split[[i]][length(split[[i]])]
}
#sample_mail$file_number<-as.character(sample_mail$file_number)
#summary(sample_mail$file_number)
#paste(as.character(c(split[[1461]][-c(1,length(split[[1461]]))])),collapse = " ")
#paste(x,collapse = " ")

two_letter_words<-scan(file.choose(), what="words")
str(two_letter_words)
sample_mail<-iconv(sample_mail,to="ASCII",sub = "")
library(tm)
sample_mail$content<-as.character(sample_mail$content)
corpus<-Corpus(VectorSource(sample_mail$content))
clean_corpus_sample<-tm_map(corpus,tolower)
clean_corpus_sample<-tm_map(clean_corpus_sample,removePunctuation)
clean_corpus_sample<-tm_map(clean_corpus_sample,removeNumbers)
removeNumPunc<-function(x)  gsub("[^[:alpha:][:space:]]*","",x)
clean_corpus_sample<-tm_map(clean_corpus_sample,content_transformer(removeNumPunc))
clean_corpus_sample<-tm_map(clean_corpus_sample,removeWords,c(stopwords(kind = "en"),two_letter_words))
clean_corpus_sample<-tm_map(clean_corpus_sample,stripWhitespace)
clean_corpus_sample$content[20]

install.packages("stringi")
library(stringi)
for (i in 1:2221) {
  sample_mail$word_count[i]<-stri_count_words(sample_mail$content[i])
  sample_mail$line_count[i]<-stri_count_boundaries(sample_mail$content[i],type="sentence")
  sample_mail$corpus_Word_count[i]<-stri_count_words(clean_corpus_sample$content[i])
}
index<-NULL
for (i in 1:2221) {
  for (j in (i+1):2221) {
    if(clean_corpus_sample$content[i]==clean_corpus_sample[j]){
      index<-c(index,j)
    }
  }
  
}
max(index)
View(index)
duplicate_index<-which(duplicated(index))
index<-index[-c(duplicate_index)]
sample_mail<-sample_mail[-c(index),]


ind<-which(duplicated(clean_corpus_sample$content))


install.packages("SnowballC")
library(SnowballC)
wordStem(clean_corpus_sample$content[20],language ="english")
