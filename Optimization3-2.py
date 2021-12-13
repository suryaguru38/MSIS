
import numpy as np

def func(x,i):
    return 300* min(x,i)-75*x

sims = []

''' The optimal number of gallons need to be between 200 and 500. 
Iterate over all possible outcomes to find the optimum
and compute and store the expected profit for each x number of gallons chosen'''
for x in range(200,501):
    sims.append(np.mean([func(x,i) for i in range(200,501)]))
                             
optima = list(range(200,501))[sims.index(max(sims))]

print(optima)

print(max(sims))