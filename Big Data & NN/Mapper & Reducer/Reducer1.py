#!/usr/bin/env python

import sys
from itertools import groupby
from operator import itemgetter

SEP = '\t'

class Reducer1(object):
    
    def __init__(self, stream = sys.stdin, sep = SEP):
        self.stream = stream
        self.sep = SEP 
               
    def __iter__(self):
        generator = (line.strip().split(self.sep, 1) for line in self.stream)
        for item in groupby(generator, itemgetter(0)):
            yield item
        
    def emit(self, key, value):
        sys.stdout.write("'{0}'{1}{2}\n".format(key,self.sep,value))
        
    def reduce(self):
        for key, group in self:
            values = set()
            for item in group: 
                values.add(item[1])
                if len(values) == 5:
                    break             
            self.emit(key, list(values))        
            
if __name__ == '__main__':
    reducer = Reducer1()
    reducer.reduce()

