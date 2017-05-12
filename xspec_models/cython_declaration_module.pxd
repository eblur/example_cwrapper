""" Cython declaration of the C extension.
"""

## VERSION 1

#cdef extern from "<valarray>" namespace "std":
#    cdef cppclass valarray

cdef extern from "minimal_powerlaw.h":
    double calcPowerLaw(const valarray& energyArray, const double& index, valarray& fluxArray);

"""
## VERSION 2

cdef extern from "xsTypes.h":
    cdef cppclass Real
    cdef cppclass RealArray:
        RealArray(size_t)
        Real& operator[](size_t pos)

cdef extern from "minimal_powerlaw.h":
    Real calcPowerLaw(const RealArray& energyArray, const Real& index, RealArray& fluxArray);
"""
