import os
import csv
import re
intpattern = '[+-]?\d+'

def remove(filename):
    with open(filename) as f:
        contents = f.read()
        m = re.findall(intpattern, contents)
        a = sum(map(int, m))
        print(contents)
        print(re.sub(r'(?!(([^"]*"){2})*[^"]*$),', '', str))

remove("/Users/morbrown/code/corpus_implicatures_binary/scripts/corpus_implicatures_binary_pilot-trials.txt")