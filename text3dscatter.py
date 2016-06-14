import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

def ThreeDim(filename,color,marker): # three dimensional
    data = np.loadtxt(filename)
    if (len(next(iter(data))) != 3):
        print("Should be three dimensional!")
        return False
    else:
        x = data[:,0]
        y = data[:,1]
        z = data[:,2]
        ax.scatter(x, y, z, color=color, marker=marker)
        ax.set_xlabel('X axis')
        ax.set_ylabel('Y axis')
        ax.set_zlabel('Z axis')
        plt.show()
        return True

def TwoDim(filename,color,marker): # two dimensional
    data = np.loadtxt(filename)
    if (len(next(iter(data))) != 2):
        print("Should be two dimensional!")
        return False
    else:
        x = data[:,0]
        y = data[:,1]
        plt.plot(x,y,color=color,marker=marker,ls=" ")
        plt.xlabel('X axis')
        plt.ylabel('Y axis')
        plt.grid(True)
        plt.show()
        return True
        
#main
ThreeDim("mysample.txt","b","o")
TwoDim("mysample.txt","r","o")