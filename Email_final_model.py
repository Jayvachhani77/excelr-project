# -*- coding: utf-8 -*-
"""
Created on Wed May 13 16:49:51 2020

@author: vw178e
"""

import numpy as np

import pandas as pd
import matplotlib.pyplot as plt

dataset = pd.read_csv('C:/Users/Priya Vachhani/Desktop/email chunks/cleaned_email_complete.csv')
df=dataset.iloc[:,3:] # removing first three columns

# removing duplicate data
df.drop_duplicates(subset='content', keep='first', inplace=True)
df['Class'].value_counts()
df.describe()

# Cleaning the texts
import re

#data cleaning
def preprocessor(text):
    text = re.sub('[^a-zA-Z]', ' ',text)
    text = re.sub('<[^>]*>', '', text)
    emoticons = re.findall('(?::|;|=)(?:-)?(?:\)|\(|D|P)', text)
    text = re.sub('[\W]+', ' ', text.lower()) +        ' '.join(emoticons).replace('-', '')
    return text

df['content']= df["content"].apply(preprocessor)

#tokenizing and lemmatizing

from nltk.stem.wordnet import WordNetLemmatizer
lem = WordNetLemmatizer() 

def tokenizer_lemmatizer(text):
    return[lem.lemmatize(word, "v") for word in text.split()]


#Stop words
from nltk.corpus import stopwords
stop= stopwords.words('english')

#creating total corpus of mails
corpus = []

for i in df.index.values:
    mail_content=[w for w in tokenizer_lemmatizer(df['content'][i]) if w not in stop]
    
    # lem = WordNetLemmatizer()
    # df['content'] = [lem.lemmatize(word, "v") for word in df['content'] if not word in set(stop)]
    mail_content = ' '.join(mail_content)
    corpus.append(mail_content)
    
# creating new cleaned dataset
new_df=pd.DataFrame(list(zip(corpus, list(df['Class']))), columns=['content', 'Class']) 


# =============================================================================
# Classifying correct labels
# =============================================================================

# cleaning data 

bad_words = []
with open("C:/Users/vw178e/Excelr_Project/bad_words.txt") as f:
    bad_words = f.read()


# splitting the entire string by giving separator as "\n" to get list of 
# all stop words
bad_words = bad_words.split("\n")

new_df['content2'] = new_df.content.apply(tokenizer_lemmatizer)
new_df.head()

# Defining function for checking and returning Abusive and Non Abusive classification
def check_abusive(input_str):
    if any ([word in input_str for word in bad_words]):
        return 'Abusive'
    return 'Non Abusive'



new_df.loc[:, 'class2'] = new_df.content2.apply(check_abusive)
new_df.head()

new_df['content2'][2]


#Comparing with orginal label with newly formed labels

new_df['class2'].value_counts()

new_df['Class'].value_counts()

# The New labels have 1448 Abusive mails and the existing labels have 1642 as Abusive mails


# Transferring new labels and dropping class2 variable

new_df['Class'] = new_df['class2']
new_df['Class'].value_counts()

new_df.drop(['class2','content2'],axis=1, inplace=True)


# =============================================================================
# Modelling
# =============================================================================

from sklearn.svm import LinearSVC

# splitting data into train and test data sets 
from sklearn.model_selection import train_test_split, cross_val_score

X = new_df['content']
y = new_df['Class']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

#Variable shapes
X_train.shape, y_train.shape, X_test.shape, y_test.shape

#Vectorization of bag of words
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
product_bow = CountVectorizer().fit(new_df.content)

X = product_bow.transform(X)

train_matrix = product_bow.transform(X_train)
train_matrix.shape

test_matrix = product_bow.transform(X_test)
test_matrix.shape

#########################################################

# Learning Term weighting and normalizing on entire emails
tfidf_transformer = TfidfTransformer().fit(X)

# Preparing TFIDF for train emails
train_matrix = tfidf_transformer.transform(train_matrix)

train_matrix.shape # (3891, 6661)

# Preparing TFIDF for test emails
test_matrix = tfidf_transformer.transform(test_matrix)

test_matrix.shape #  (4416, 25319)

#Model Evaluation
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.metrics import precision_score, recall_score, f1_score
from sklearn.metrics import plot_roc_curve




### SVM
svm_model = LinearSVC()
svm_model.fit(train_matrix,y_train)
train_pred_svc = svm_model.predict(train_matrix)
accuracy_train_svc = np.mean(train_pred_svc==y_train) # 96.55%

y_preds = svm_model.predict(test_matrix)
accuracy_test_svc = np.mean(y_preds==y_test)

print(classification_report(y_test,y_preds))
pd.crosstab(y_test,y_preds)


# Import Seaborn
import seaborn as sns
sns.set(font_scale=1.5) # Increase font size

def plot_conf_mat(y_test, y_preds):
    """
    Plots a confusion matrix using Seaborn's heatmap().
    """
    fig, ax = plt.subplots(figsize=(3, 3))
    ax = sns.heatmap(confusion_matrix(y_test, y_preds),
                     annot=True, # Annotate the boxes
                     cbar=False)
    plt.xlabel("true label")
    plt.ylabel("predicted label")
    bottom, top = ax.get_ylim()
    ax.set_ylim(bottom+0.5, top - 0.5)

plot_conf_mat(y_test, y_preds)    

#Plot ROC curve and calculate Auc metric
plot_roc_curve(svm_model,test_matrix,y_test)

