#!/usr/bin/env python

import sys
import os
import string

SEP = '\t'
vowels = ['a', 'e', 'i', 'o', 'u']
STOP_WORDS = ['i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', "you're", "you've", "you'll", "you'd", 'your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his', 'himself', 'she', "she's", 'her', 'hers', 'herself', 'it', "it's", 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 'who', 'whom', 'this', 'that', "that'll", 'these', 'those',  'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 'don', "don't", 'should', "should've", 'now', 'd', 'll', 'm', 'o', 're', 've', 'y', 'ain', 'aren', "aren't", 'couldn', "couldn't", 'didn', "didn't", 'doesn', "doesn't", 'hadn', "hadn't", 'hasn', "hasn't", 'haven', "haven't", 'isn', "isn't", 'ma', 'mightn', "mightn't", 'mustn', "mustn't", 'needn', "needn't", 'shan', "shan't", 'shouldn', "shouldn't", 'wasn', "wasn't", 'weren', "weren't", 'won', "won't", 'wouldn', "wouldn't"]

class Mapper(object):
    
    def __init__(self, stream = sys.stdin, sep = SEP):
        self.stream = stream
        self.sep = SEP
        self.stop_words = STOP_WORDS 
        self.exclude_chars = string.digits + string.punctuation
               
    def __iter__(self):
        for row in self.stream:
            yield row.strip()
    
    def remove_punct_dig(self, line):
        table = string.maketrans(self.exclude_chars, len(self.exclude_chars) * ' ')
        return line.translate(table)
    
    def normalize(self, token):
        return token.lower()
    
    def exclude(self, token): 
        return (token in self.stop_words)
    
    def word_check(self, token):
        return (token[:1] in vowels and len(token) > 6)
    
    def preprocess(self, line):
        line = self.remove_punct_dig(line)
        for token in line.split():
            token = self.normalize(token)
            if not self.exclude(token) and self.word_check(token):
                yield token
    
    def emit(self, key, value):
        sys.stdout.write('{0}{1}{2}\n'.format(key,self.sep,value))
                
    def mapping(self):
        for line in self:
            for key in self.preprocess(line):
                file_path = os.getenv('map_input_file')
                file_name = file_path.strip().split('/')[-1]
                self.emit(key, file_name)         

if __name__ == '__main__':
    mapper = Mapper()
    mapper.mapping()
