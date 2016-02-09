import os
import csv
import nltk
from childes import CHILDESCorpusReader

# Takes a language, returns a CHIDLESCorpusReader for that language
def get_corpus_reader(language):
    return CHILDESCorpusReader(corpus_root, r'%s.*/.*\.xml' % language[:3].title())

# Takes a fileid, gets counts of all the words for that file, writes a csv
def get_file_counts(corpus_reader, corpus_file):
    age = corpus_reader.age(corpus_file, month=True)[0]
    sex = corpus_reader.sex(corpus_file)[0]
    corpus_participants = corpus_reader.participants(corpus_file)[0]
    not_child = [value['id'] for key, value in corpus_participants.iteritems() if key != 'CHI']
    corpus_words = corpus_reader.words(corpus_file, speaker=not_child, replace=True)
    freqs = nltk.FreqDist(corpus_words)
    filename = 'data/%s_%s_%s.csv' % (os.path.basename(corpus_file).split('.')[0], age, sex)
    writer = csv.writer(open(filename, 'w'))
    writer.writerow(["word", "count"])
    for word, count in freqs.iteritems():
        writer.writerow([word, count])

# Takes a language, writes csvs for the word counts of each file in that language
def get_lang_counts(language):
    corpus_reader = get_corpus_reader(language)
    for corpus_file in corpus_reader.fileids():
        get_file_counts(corpus_reader, corpus_file)

#corpus_root = nltk.data.find('corpora/childes/data-xml/')
corpus_root = nltk.data.find('corpora/childes/data-xml/English/Eng-NA-MOR/Bloom70')
corpus_reader = CHILDESCorpusReader(corpus_root, r'.*/.*\.xml')
for f in corpus_reader.fileids():
    get_file_counts(corpus_reader, f)
