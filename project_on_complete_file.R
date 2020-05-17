#reading csv file containing all the emails
project_org_csv<-read.csv(choose.files())

#reading text file containing one and two letter words which is used during cleaning corpus
two_letter_words<-scan(file.choose(), what="words")

#creating necessary attributes column filled with NA values
project_org_csv$name<-NA
project_org_csv$file_type<-NA
project_org_csv$file_number<-NA
project_org_csv$word_count<-NA
project_org_csv$line_count<-NA
project_org_csv$corpus_Word_count<-NA
project_org_csv$receivers<-NA


#importing "tm" library to perform text mining on the data
library(tm)

#converting the data type of content to character so that it can be converted to corpus and tm can be used on it.
project_org_csv$content<-as.character(project_org_csv$content)

#creating corpus and cleaning corpus
full_corpus<-Corpus(VectorSource(project_org_csv$content))
clean_corpus<-tm_map(full_corpus,tolower)
clean_corpus<-tm_map(clean_corpus,removePunctuation)
clean_corpus<-tm_map(clean_corpus,removeNumbers)
removeNumPunc<-function(x)  gsub("[^[:alpha:][:space:]]*","",x)
clean_corpus<-tm_map(clean_corpus,content_transformer(removeNumPunc))
clean_corpus<-tm_map(clean_corpus,removeWords,c(stopwords(kind = "en"),two_letter_words))
clean_corpus<-tm_map(clean_corpus,stripWhitespace)
clean_corpus<-tm_map(clean_corpus,stemDocument)
#checking the content of 20th mail for verification
clean_corpus$content[20]

#storing the index position of mails which are exact duplicate of previous mails after cleaning corpus
full_ind<-which(duplicated(clean_corpus$content))

#deleting the rows having index location in full_ind vector to get the final dataset having no duplicate mail content.
project_org_csv<-project_org_csv[-c(full_ind),]

#changing data type of filename
project_org_csv$filename<-as.character(project_org_csv$filename)

#spliting the content of file name 
split_complete_file<-strsplit(project_org_csv$filename,"/")

#filling the splitted values of filename into three newly created variables
for (i in 1:nrow(project_org_csv)) {
  project_org_csv$name[i]<-split_complete_file[[i]][1]
  project_org_csv$file_type[i]<-paste(as.character(c(split_complete_file[[i]][-c(1,length(split_complete_file[[i]]))])),collapse = " ")
  project_org_csv$file_number[i]<-split_complete_file[[i]][length(split_complete_file[[i]])]
}

#deleting the first 3 columns. column 1&3 are useless and content of column 2 we got by splitting it 
#in 3 different columns
project_org_csv<-project_org_csv[,-c(1,2,3)]

#installing "stringi" package to perform string related operations on cleaned corpus.
#install.packages("stringi")
library(stringi)

#counting no. of words before cleaning content, counting no. of words after cleaning content and 
#counting no. of sentences before cleaning.
#sentence are not counted after cleaning because as punctuations are removed, entire content is within a single sentence
for (i in 1:nrow(project_org_csv)) {
  project_org_csv$word_count[i]<-stri_count_words(project_org_csv$content[i])
  project_org_csv$line_count[i]<-stri_count_boundaries(project_org_csv$content[i],type="sentence")
  project_org_csv$corpus_Word_count[i]<-stri_count_words(clean_corpus$content[i])
}

#creating a corpus to just remove the blank space to extract receivers of emails
corpus_for_receivers<-Corpus(VectorSource(project_org_csv$content))

#removing blank space from the corpus
corpus_for_receivers<-tm_map(corpus_for_receivers,stripWhitespace)

#storing the content of corpus without only whitespaces into content variable
content<- as.character(corpus_for_receivers$content)

#declaring few variables to extract receivers names
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
#checking the extracted receivers names
print(reqNames[1:100])

#few mails has multiple receivers. So splitting them with ","
split_receivers<-strsplit(reqNames,",")

#checking the maximum numbers of receivers of a mail and which mail contains it.
which.max(lengths(split_receivers))
max(lengths(split_receivers))

#declaring a list to store splitted part of user name to remove unnecessary part later step 
split_receivers2<-list()

#splitting the extracted names with "/"
for (i in 1:length(split_receivers)) {
  for (j in 1:(length(split_receivers[[i]]))) {
    split_receivers2[[i]]<-strsplit(split_receivers[[i]],"/")  
  }
}
#checking the results
split_receivers2[1:20]

#declaring a multilevel list
split_receivers3<-vector(mode = "list",length(split_receivers))

#only storing the first part of splitted list
for (i in 1:length(split_receivers)) {
  for (j in 1:(length(split_receivers[[i]]))) {
    split_receivers3[[i]][[j]]<-split_receivers2[[i]][[j]][1]
  }
}
#checking result
split_receivers3[1:20]

#mapping the extracted names with their respective mail rows.
for (i in 1:(length(uid))) {
  project_org_csv$receivers[uid[i]]<-paste(split_receivers3[[i]],collapse = " ,")
}

#setting the NA values of receviers as "unknown"
project_org_csv$receivers<-ifelse(is.na(project_org_csv$receivers)==TRUE,"unknown",project_org_csv$receivers)


project_org_csv$name<-as.factor(project_org_csv$name)
project_org_csv$file_type<-as.factor(project_org_csv$file_type)
project_org_csv$file_number<-as.integer(project_org_csv$file_number)
project_org_csv$receivers<-as.factor(project_org_csv$receivers)
str(project_org_csv)

#creating a csv file after EDA  
write.csv(project_org_csv,file = "C:/Users/Priya Vachhani/Desktop/email chunks/EDA_FILE.csv",row.names = FALSE)
##########################creating TDM and DTM###############################
#reading the EDA file to check
clean_project_mails<-read.csv(choose.files())

#removing the duplicate elements from the clean_corpus to get the clean_corpus_final
clean_corpus_final<- Corpus(VectorSource((clean_corpus$content[-c(full_ind)])))
#clean_corpus_final<-tm_map(clean_corpus_final,removeWords,c(stopwords(kind = "en"),common_terms_dictionary))

#creating DTM and TDM on clean_corpus_final
DTM_on_complete<-DocumentTermMatrix(clean_corpus_final)
TDM_on_complete<-TermDocumentMatrix(clean_corpus_final)
#creating a dictionary of frequent terms where minimum frequency is set to 10
dictionary<-findFreqTerms(DTM_on_complete,10)
write(dictionary,file = "C:/Users/Priya Vachhani/Desktop/email chunks/dictionary.txt",sep=" ")


dict_DTM<-DocumentTermMatrix(clean_corpus_final,list(dictionary=dictionary))
convert_count<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels = c(0,1),labels = c("NO","YES"))
}
dict_DTM<-apply(dict_DTM,MARGIN = 2,convert_count)





#################################no use since we are doing unsupervised learning################
#####################creating and saving new classification based DTMs and dictionaries################################
dtm_abusive<-DocumentTermMatrix(clean_corpus_final[1:1553])
dtm_non_abusive<-DocumentTermMatrix(clean_corpus_final[1554:22814])
dictionary_abusive<-findFreqTerms(dtm_abusive,5)
dictionary_non_abusive<-findFreqTerms(dtm_non_abusive,10)
common_terms_dictionary<-intersect(dictionary_abusive,dictionary_non_abusive)
write(common_terms_dictionary,file = "C:/Users/Priya Vachhani/Desktop/email chunks/common_terms_dictionary.txt",sep = " " )
write(dictionary_abusive,file = "C:/Users/Priya Vachhani/Desktop/email chunks/abusive_dictionary.txt",sep=" ")
write(dictionary_non_abusive,file = "C:/Users/Priya Vachhani/Desktop/email chunks/non_abusive_dictionary.txt",sep=" ")
common_terms_dictionary<-scan(choose.files(), what = "words")

