# ASCII file handling
import numpy as np

def TxtRead(filename):
    data = np.loadtxt(filename)
    print(data)
#    file = open(filename,"r") # with default libraries
#    content = file.read()
#    print(content)
#    file.close()
    
def TxtFindFirst(value, filename):
    data = np.loadtxt(filename)
    rowstotal = len(data)
    columnstotal = len(next(iter(data)))
    stop = False
    for i in range(0,rowstotal):
        for j in range(0,columnstotal):
            if(data[i][j] == value):
                print(str(value) + " is found at (" + str(i) + "," + str(j)+")")
                stop = True;
                break
        if(stop == True):
            break
    if(stop == False):
        print(str(value) + " is not found")

def TxtFindAll(value, filename): #returns list
    data = np.loadtxt(filename)
    rowstotal = len(data)
    columnstotal = len(next(iter(data)))
    result= []
    amount = 0
    for i in range(0,rowstotal):
        for j in range(0,columnstotal):
            if(data[i][j] == value):
                amount += 1
                result.append([i,j])
    print("Total " + str(amount) + " values found.")
    return (result)

def TxtEditRow(rowindex,newvalue,filename,savechanges): 
    data = np.loadtxt(filename)
    rowstotal = len(data)
    columnstotal = len(next(iter(data)))
    edited = data # store original values and form a new array
    if (rowindex > rowstotal):
        print("Index out of range!")
        return False
    elif (len(newvalue) != columnstotal):
        print("Dimensions must agree!")
        return False
    else:
        print("Old value = " + str(edited[rowindex]))
        edited[rowindex] = newvalue
        print("New value = " + str(edited[rowindex]))
        if (savechanges =="y" or savechanges =="Y"):
            np.savetxt("output.txt", edited)
            print("Changes are saved to output.txt")
        return edited

def TxtSort(filename,savechanges):  # Descending order
    data = np.loadtxt(filename)
    rowstotal = len(data)
    columnstotal = len(next(iter(data)))
    mysorted = np.zeros((rowstotal,columnstotal)) # initialize with zeros
    TrueList = []
    for i in range(0,rowstotal):
        for j in range(0,columnstotal):
            TrueList.append(data[i][j])
    for j in range(0,len(TrueList)-1):  # Bubble sort algorithm
        for i in range(0,len(TrueList)-1):
            if (TrueList[i] < TrueList[i+1]):
                temp = TrueList[i]
                TrueList[i] = TrueList[i+1]
                TrueList[i+1] = temp
    n = 0
    for i in range(0,rowstotal):
        for j in range(0,columnstotal):
            mysorted[i][j] = TrueList[n]
            n+=1
    if (savechanges =="y" or savechanges =="Y"):
        np.savetxt("output.txt", mysorted)
        print("Changes are saved to output.txt")
    return mysorted
    
def TxtConcatenate(filename1,filename2): # rowwise
    data1 = np.loadtxt(filename1)
    data2 = np.loadtxt(filename2)
    rowstotal = [len(data1),len(data2)]
    columnstotal = [len(next(iter(data1))),len(next(iter(data2)))]
    if (columnstotal[0] != columnstotal[1]):
        print("Dimensions must agree!")
        return -1
    Result = np.zeros((sum(rowstotal),columnstotal[0])) # initialize with zeros
    for i in range(0,rowstotal[0]):
        Result[i,:] = data1[i,:]
    for i in range(rowstotal[0],sum(rowstotal)):
        Result[i,:] = data2[i-rowstotal[0],:]
    return Result

#main
TxtRead("output.txt")
#TxtFindFirst(69,"mysample.txt")
#print(TxtFindAll(69,"mysample.txt"))
TxtEditRow(1,[213,213,213],"mysample.txt","n")
#print(TxtSort("mysample.txt","y"))
#print(TxtConcatenate("mysample.txt","mysample2.txt"))