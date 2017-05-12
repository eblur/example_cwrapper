cimport numpy as np
cimport cython_declaration_module
import numpy as np


## VERSION 1

cdef extern from "<valarray>" namespace "std":
    cdef cppclass valarray[T]:
        valarray()
        valarray(int)  # constructor: empty constructor
        T& operator[](int)  # get/set element

def calculate_powerlaw(double[:] energyArray, alpha):

    cdef valarray[double] c_energyArray
    cdef valarray[double] c_result
    cdef int i

    c_energyArray = valarray[double](len(energyArray))
    for i in range(len(energyArray)):
        c_energyArray[i] = energyArray[i]

    c_result = valarray[double](len(energyArray))
    cython_declaration_module.calcPowerLaw(c_energyArray, alpha, c_result)

    result   = np.empty(len(energyArray), dtype=np.double)
    for i in range(len(energyArray)):
        result[i] = c_result[i]

    return result


"""
## VERSION 2

cdef extern from "xsTypes.h":
    cdef cppclass Real
    cdef cppclass RealArray[T]:
        RealArray()
        RealArray(int)  # constructor: empty constructor
        T& operator[](int)  # get/set elements

def calculate_powerlaw(double[:] energyArray, alpha):

    cdef RealArray[Real] c_energyArray
    cdef RealArray[Real] c_result
    cdef int i

    c_energyArray = RealArray[Real](len(energyArray))
    for i in range(len(energyArray)):
        c_energyArray[i] = energyArray[i]

    c_result = RealArray[Real](len(energyArray))
    cython_declaration_module.calcPowerLaw(c_energyArray, alpha, c_result)

    result   = np.empty(len(energyArray), dtype=np.double)
    for i in range(len(energyArray)):
        result[i] = c_result[i]

    return result
"""

"""
## SCRATCH

    cdef RealArray* c_energyArray
    c_energyArray = new RealArray(len(energyArray))
    cdef Real* ptr
    ptr = <Real*>&c_energyArray[0]
    cdef int i
    for i in range(len(energyArray)):
        ptr[i] = 0.0
        #energyArray[i]

    #cython_declaration_module.calcPowerLaw(c_energyArray, alpha, result)
    #return result

cdef extern from "xsTypes.h":
    cdef cppclass Real
    cdef cppclass RealArray:
        RealArray(size_t)
        Real& operator[](size_t pos)

"""
