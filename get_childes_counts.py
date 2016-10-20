import os
import nltk
from childes import CHILDESCorpusReader
from unicode_csv import *

# Takes a language, returns a CHIDLESCorpusReader for that language
def get_corpus_reader(language):
    return CHILDESCorpusReader(corpus_root, r'%s.*/.*\.xml' % language[:3].title())

# Takes a fileid, gets counts of all the words for that file, writes a csv
def get_file_counts(corpus_reader, corpus_file, meta_writer):
    base_directory = os.path.dirname(corpus_file)
    directory = os.path.join('data', base_directory)
    if not os.path.exists(directory):
        os.makedirs(directory)
    base_file = os.path.join(base_directory, os.path.basename(corpus_file).split('.xml')[0])
    filename = os.path.join('data', base_file + '.csv')
    if os.path.isfile(filename):
        print "Count file for %s already exists, skipping" % corpus_file
    else:
        print "Getting counts for %s" % corpus_file
        sex = corpus_reader.sex(corpus_file)[0]
        age = corpus_reader.age(corpus_file, month=True)[0]
        meta_writer.writerow([base_file, str(sex), str(age)])
        corpus_participants = corpus_reader.participants(corpus_file)[0]
        not_child = [value['id'] for key, value in corpus_participants.iteritems() if key != 'CHI']
        corpus_words = corpus_reader.words(corpus_file, speaker=not_child, replace=True, stem=True)
        freqs = nltk.FreqDist(corpus_words)
        writer = UnicodeWriter(open(filename, 'w'))
        writer.writerow(["word", "count"])
        for word, count in freqs.iteritems():
            try:
                writer.writerow([word, str(count)])
            except:
                print "couldn't write word %s with count %d" % (word, count)


corpus_root = nltk.data.find('corpora/childes/data-xml/English/Eng-NA-MOR')
corpus_reader = CHILDESCorpusReader(corpus_root, r'.*/.*\.xml')
meta_writer = UnicodeWriter(open('data/metadata.csv', 'a'))
meta_writer.writerow(["filename", "sex", "age"])
for f in corpus_reader.fileids():
    get_file_counts(corpus_reader, f, meta_writer)
