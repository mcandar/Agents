import numpy as np
import timeit

b = np.zeros((10,10))

n = 1
for i in range(0,10):
    for j in range(0,10):
        b[i,j] = n
        n = n + 1
print(b)
c = b

start = timeit.timeit()
for i in range(0,10^5):
    d = np.dot(b,c)
end = timeit.timeit()
print ("10^5 number of simulations take ", end - start, " seconds.")
