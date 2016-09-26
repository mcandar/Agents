import numpy as np
import time

b = np.zeros((10,10))

n = 1
for i in range(0,10):
    for j in range(0,10):
        b[i,j] = n
        n = n + 1
c = b

start = time.clock()
for i in range(0,10^5):
    d = np.dot(b,c)
end = time.clock()
print(d)
print ("10^5 number of simulations take ", end - start, " seconds.")
