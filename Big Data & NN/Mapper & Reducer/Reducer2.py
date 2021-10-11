#!/usr/bin/env python

import sys
from itertools import groupby
from operator import itemgetter

SEP = '\t'

class Reducer2(object):
    
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
	    files = [item[1] for item in group]
            file_counts = ((file, files.count(file)) for file in set(files))
            self.emit(key, dict(file_counts))
            
if __name__ == '__main__':
    reducer = Reducer2()
    reducer.reduce()
