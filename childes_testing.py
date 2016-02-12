import nltk
from nltk.corpus.reader.xmldocs import ElementTree

NS = 'http://www.talkbank.org/ns/talkbank'
xstr = lambda s: "" if s is None else str(s)
corpus_root = nltk.data.find('corpora/childes/data-xml/English/Eng-NA-MOR/')

reload(childes)
from childes import CHILDESCorpusReader
corpus_reader = CHILDESCorpusReader(corpus_root, r'Providence/William/wil11.xml')
words = corpus_reader.words(replace=True)
freqs = nltk.FreqDist(words)

fileid = corpus_reader.fileids()[0]
xmldoc = ElementTree.parse(corpus_root + "/" + fileid).getroot()
xmlsents = xmldoc.findall('.//{%s}u' % NS)

xmlsent = xmlsents[173]
xmlwords = xmlsent.findall('.//{%s}w' % NS)
xmlword = xmlwords[2]

shortenings = xmlword.findall('.//{%s}shortening' % (NS))
text = xstr(xmlword.text) + "".join([xstr(short.text) + xstr(short.tail) for short in shortenings])