# -*- coding: utf-8 -*-
"""
Created on Thu Jun  2 21:22:38 2016

@author: snerur
"""

import textblob as blob
import nltk
import string
import numpy as np
import spacy

def get_pos(word):
    nlp_en = spacy.load('en_core_web_sm')
    token = nlp_en(word)
    return token[0].pos_

def make_verb(text, pos):
    return text if pos == "VERB" else text

def is_noun(tag):
    return tag == 'NN' or tag == 'NNS'

def is_adjective(tag):
    return tag == 'JJ' or tag == 'JJR' or tag == 'JJS'

def is_adverb(tag):
    return tag == "RB" or tag == "RBR" or tag == "RBS"

def is_verb(tag):
    aList = ["VB", "VBD", "VBG", "VBN", "VBP", "VBZ"]
    return tag in aList

def get_POS(tag):
    if is_noun(tag):
        return "NOUN"
    if is_adjective(tag):
        return "ADJ"
    if is_adverb(tag):
        return "ADV"
    if is_verb(tag):
        return "VERB"
    
def convert_to_nouns(txt):
    temp_list = []
    #t_blob = blob.TextBlob(txt)
    for item in nltk.pos_tag(txt.split()):
        i = str(item[1])
        if is_noun(i):
            temp_list.append(str(item[0]))
    return " ".join(temp_list)
    
def convert_to_adjectives(txt):
    temp_list = []
    #t_blob = blob.TextBlob(txt)
    for item in nltk.pos_tag(txt.split()):
        i = str(item[1])
        if is_adjective(i):
            temp_list.append(str(item[0]))
    return " ".join(temp_list)

def convert_to_nouns_and_adjectives(txt):
    temp_list = []
    #t_blob = blob.TextBlob(txt)
    for item in nltk.pos_tag(txt.split()):
        i = str(item[1])
        if is_noun(i) or is_adjective(i):
            temp_list.append(str(item[0]))
    return " ".join(temp_list)
    

def parse(txt, punct = True, numbers = True, stemmer = False, lemmatize = True, stopword_list = []):
    """parameter list = text, punct, numbers, stemmer"""
    txt = txt.lower()
    #remove punctuation if necessary
    if punct:
        p = string.punctuation
        tbl = str.maketrans(p, len(p) * " ")
        txt = txt.translate(tbl)
    if numbers:
        d = string.digits
        tbl = str.maketrans(d, len(d) * " ")
        txt = txt.translate(tbl)
    
    #remove stopwords
    stopwords = nltk.corpus.stopwords.words('english')
    stopwords.extend(stopword_list)
    stopwords = np.array(stopwords).astype(str)
    word_list = [word for word in txt.split() if word not in stopwords and len(word) > 2]
    
    
    #lemmatize by default
    if lemmatize:
        txt = " ".join(word_list)
        nlp_en = spacy.load('en_core_web_sm')
        docs = nlp_en(txt)
        word_list = [doc.lemma_ for doc in docs]
    
    #stem if necessary
    if stemmer:
        s = nltk.PorterStemmer()
        word_list = [s.stem(word) for word in word_list]
    
    
    txt = " ".join(word_list)
    txt = txt.replace("-PRON-","")
    txt = txt.replace("PRON","")
    
    return txt
    
    