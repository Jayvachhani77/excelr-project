sample_mail2<-read.csv(choose.files())
sample_mail2$receivers<-NA
sample_mail2<-sample_mail2[,-c(1,2,3,5)]
sample_mail2$content<-as.character(sample_mail2$content)
corpus2<-Corpus(VectorSource(sample_mail2$content))
clean_corpus_sample<-tm_map(corpus2,stripWhitespace)

content <- as.character(clean_corpus_sample$content)

reqNames <- ""
uid <- 0
st <- 1

for (i in 1:length(content)){
  
  t <- 0
  c <- 0
  s <- 0
  tos <- " "
  ccs <- " "
  
  if (stri_detect_fixed(content[i], c("To:", "c:", "Subject:"))){
    
    t <- as.numeric(head(stri_locate_first_fixed(content[i], 'To:')[[1]], 1))
    s <- as.numeric(head(stri_locate_first_fixed(content[i], 'Subject:')[[1]], 1))
    
    if (stri_detect_fixed(content[i], c("c:"))){
      c <- as.numeric(head(stri_locate_first_fixed(content[i], 'c:')[[1]], 1))
      tos <- substr(content[i], t+3, c-2)
      ccs <- substr(content[i], c+3, s-2)
    }
    else if (stri_detect_fixed(content[i], c("Subject:")) & s>t){
      tos <- substr(content[i], t+3, s-2)
    }
    else{
      el <- as.numeric(head(stri_locate_last_fixed(content[i], ':')[[1]], 1))
      tos <- substr(content[i], t+3, el-2)
    }
    
    reqNames[st] <- stri_c(tos, ccs, sep=";")
    uid[st] <- i
    st <- st + 1
  }
}
print(reqNames)
split_receivers<-strsplit(reqNames,",")

which.max(lengths(split_receivers))
reqNames[233]
for (i in 1:625) {
  for (j in 1:(length(split_receivers[[i]]))) {
    split_receivers2[[i]]<-strsplit(split_receivers[[i]],"/")  
    }
}
split_receivers2
split_receivers3<-NA
split_receivers3<-as.list(split_receivers3)
str(split_receivers3)
for (i in 1:625) {
  for (j in 1:(length(split_receivers[[i]]))) {
    split_receivers3[[i]][[j]]<-split_receivers2[[i]][[j]][1]
  }
}
split_receivers3[[233]]
for (i in 1:(length(uid))) {

    sample_mail2$receivers[uid[i]]<-paste(split_receivers3[[i]],collapse = " ,")
    
}



uid[2]
split_receivers[[11]][1]
reqNames[11]
length(split_receivers[[11]])
strsplit(split_receivers[[11]],"/") 
paste(split_receivers3[[11]],collapse = " ,")
