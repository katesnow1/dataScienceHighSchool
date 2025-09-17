import csv
import math
import os
import string
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
import re

def kmeans(data, k, epochs):
  centers = []
  clusters = []
  for i in range(k):
    centers.append(data[i][1:])
    clusters.append([])
  for i in range(epochs):
    for j in range(k):
      clusters[j] = []
    for point in data:
      centerIndex = distanceCenter(point[1:], centers)
      clusters[centerIndex].append(point)
    for l in range(k):
      centers[l] = avgCenter(clusters[l])
  return clusters

def distanceCenter(point, centers):
  minDis = 0
  for k in range(len(point)):
    minDis += (point[k] - centers[0][k]) ** 2
  minDis = minDis ** 0.5
  #minDis = (((point[0] - centers[0][0]) ** 2) + ((point[1] - centers[0][1]) ** 2)) ** 0.5
  index = 0
  for i in range(1, len(centers)):
    #d = (((point[0] - centers[i][0]) ** 2) + ((point[1] - centers[i][1]) ** 2)) ** 0.5
    d = 0
    for k in range(len(point)):
      d += (point[k] - centers[i][k]) ** 2
    d = d ** 0.5
    if (d < minDis):
      minDis = d
      index = i
  return index

def avgCenter(cluster):
  mean = []
  for i in range(len(cluster[0]) - 1):
    avg = 0
    for point in cluster:
      avg += point[i + 1]
    avg = avg / len(cluster)
    mean.append(avg)
  return mean

def csvConvert(clusters):
  myFile = open('clusters.csv', 'w')
  writer = csv.writer(myFile)
  writer.writerow(['Cluster', 'X', 'Y'])
  for i in range(len(clusters)):
    for j in range(len(clusters[i])):
      row = [i + 1, clusters[i][j][0], clusters[i][j][1]]
      writer.writerow(row)
  myFile.close()

def readCSV(fileName): #reads the data from the csv file
  points = []
  first = True
  with open(fileName, newline = '') as f:
    reader = csv.reader(f)
    for row in reader:
        if(first):
          first = False
        else:
          points.append([float(row[0]), float(row[1])])
  return points

def tf(t, f): #term frequency
  count = f.count(t)
  wordCount = len(f.split())
  return count / wordCount

def idf(t, D): #inverse document frequency
  numOfDocs = 0
  for d in D:
    for path in os.listdir(d):
      numOfDocs += 1
  numOfDocsWithTerm = 0
  for d in D:
    for filename in os.listdir(d):
      f = open(d + "/" + filename, "r", encoding = "ISO-8859-1")
      f = removePunc(f.read())
      f = f.lower()
      if(f.count(t) > 0):
        numOfDocsWithTerm += 1
  return math.log((numOfDocs / numOfDocsWithTerm), math.e)

def tfidf(t, d, D):
  for dir in D:
    if os.path.exists(dir + "/" + d):
      f = open(dir + "/" + d, "r", encoding = "ISO-8859-1")
  f = removePunc(f.read())
  f = f.lower()
  return tf(t, f) * idf(t, D)

def removePunc(s):
  for c in string.punctuation:
    s = s.replace(c, "")
  return s

def mostFreq(folders, n):
  dict = {}
  for folder in folders:
    for filename in os.listdir(folder):
      f = open(folder + "/" + filename, "r", encoding = "ISO-8859-1")
      f = removePunc(f.read())
      f = f.lower()
      #f = removeHeaders(f)
      #f = removeStopWords(f)
      #f = removeEmailURL(f)
      for word in f.split():
        if word in dict:
          dict[word] += 1
        else:
          dict.update({word: 1})
  lstTerms = sorted(dict.items(), key = lambda x:x[1], reverse = True)
  return lstTerms[0:n]
  


def doctfidf(freqWords):
  wordtfidf = []
  D = ["comp.graphics", "rec.autos", "sci.electronics"]
  for d in D:
    for filename in os.listdir(d)[:20]:
      lst = []
      for i in range(len(freqWords)):
        lst.append(tfidf(freqWords[i][0], filename, D))
      wordtfidf.append([d + "/" + filename] + lst)
  return wordtfidf
  


#nltk.download('stopwords')
#nltk.download('punkt')
def removeStopWords(txt):
  stopWords = set(stopwords.words('english'))
  words = word_tokenize(txt)
  filteredTXT = ""
  for w in words:
    if w not in stopWords:
        filteredTXT = filteredTXT + " " + w
  return filteredTXT

def removeHeaders(txt):
  return txt[txt.index("\n\n"):]

def removeEmailURL(txt):
  newTXT = re.sub(r'([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]+\.[a-zA-Z0-9_-]+)', "", txt)
  return re.sub(r'http\S+', "", newTXT)

freqWords = mostFreq(["comp.graphics", "rec.autos", "sci.electronics"], 10)
#print(freqWords)
wordtfidf = doctfidf(freqWords)
clustersForums = kmeans(wordtfidf, 3, 25)
#print(clustersForums)
for cluster in clustersForums:
  print("Cluster:")
  for doc in cluster:
    print(doc[0])

for cluster in clustersForums:
  print("Cluster:")
  print(len(cluster))
  compCount = 0
  autosCount = 0
  sciCount = 0
  for doc in cluster:
    print(doc[0])
    if doc[0][0] == "c":
      compCount += 1
    elif doc[0][0] == "r":
      autosCount += 1
    else:
      sciCount += 1
  print("comp.graphics")
  #print(compCount / len(cluster))
  print(compCount / 20)
  print("rec.autos")
  #print(autosCount / len(cluster))
  print(autosCount / 20)
  print("sci.electronics")
  #print(sciCount / len(cluster))
  print(sciCount / 20)