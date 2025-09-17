import random
from statistics import mean
import csv
def calculatePoint(theta, row):
  y = 0
  i = 0
  for num in row:
    y += theta[i] * num
    i += 1
  y += theta[-1]
  return y

def error(theta, dataset):
  sum = 0
  i = 0
  for row in dataset[1]:
    sum += (calculatePoint(theta, row) - dataset[0][i])**2
    i += 1
  return sum

def derivative(theta, dataset, epsilon, index):
  addTheta = theta.copy()
  addTheta[index] = addTheta[index] + epsilon
  subTheta = theta.copy()
  subTheta[index] = subTheta[index] - epsilon
  numerator = error(addTheta, dataset) - error(subTheta, dataset)
  denominator = epsilon * 2
  return (numerator / denominator)

def gradient(theta, dataset, epsilon):
  gradientList = []
  for i in range(len(theta)):
    gradientList.append(derivative(theta, dataset, epsilon, i))
  return gradientList

def linearRegressionGD(dataset, epochs, gamma, epsilon):
  #theta = []
  #for i in range(len(dataset[1][0])):
    #theta.append(10)
  #theta.append(10)
  theta = [0.36, 1.05, 138]
  for j in range(epochs):
    for i in range(len(theta)):
      gradients = gradient(theta, dataset, epsilon)
      theta[i] = theta[i] - gamma * gradients[i]
    print (theta)
  return theta

def generateDataSet(slopes, intercept, numOfPoints):
  dataset = ([], [])
  for i in range(numOfPoints):
    point = []
    for j in range(len(slopes)):
      num = random.randint(0, 10)
      point.append(i + num)
    dataset[1].append(point)
  slopes.append(intercept)
  for i in range(numOfPoints):
    value = 0
    value = calculatePoint(slopes, dataset[1][i])
    dataset[0].append(value)
  return dataset

def rsquared(dataset, theta):
  varRes = error(theta, dataset)
  mean1 = mean(dataset[0])
  sum = 0
  for i in dataset[0]:
    sum += (i - mean1)**2
  r = 1 - (varRes / sum)
  return r

def readCSV(fileName): #reads the data from the csv file
  dataset = ([], [])
  with open(fileName, newline = '') as f:
    reader = csv.reader(f)
    b = True
    for row in reader: #this just skips over the first row (the county names)
      if b:
        b = False
      else:
        dataset[0].append(float(row[1]))
        values = []
        for i in range(len(row) - 2):
          values.append(float(row[i + 2]))
        dataset[1].append(values)
  return dataset
  


#dataset = generateDataSet([-4, 2, 10], 3, 10) #Equation: z = -4x + 2y + 10w + 3
#datasetUS = readCSV("CancerDataCleanTrainingSet.csv")
#datasetMA = readCSV("CancerDataMA.csv")

  #for i in datasetMA[1]:
#  print(calculatePoint(theta, i))
  
#datasetUS2 = readCSV("CancerDataCleanTrainingSet.csv")
#for i in datasetUS2[1]: #poverty and pctblack
#  i.pop(2)
#theta = linearRegressionGD(datasetUS2, 100, 0.0000005, 1)

#datasetUS3 = readCSV("CancerDataCleanTrainingSet.csv")
#for i in datasetUS3[1]: #poverty and coverage
#  i.pop(1)
#theta = linearRegressionGD(datasetUS3, 100, 0.0000005, 1)

#datasetUS4 = readCSV("CancerDataCleanTrainingSet.csv")
#for i in datasetUS4[1]: #pctblack and coverage
#  i.pop(0)
#theta = linearRegressionGD(datasetUS4, 100, 0.00000005, 1)

#r2 = rsquared(datasetUS4, theta)
#print (r2)
