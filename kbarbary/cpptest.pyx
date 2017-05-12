"""example of converting a numpy array to a C++ valarray and back in cython
using copies.

Compile with `python setup.py build_ext --inplace`

Example
-------
>>> import cpptest, numpy as np

>>> x = np.array([0., 1., 2.])

>>> cpptest.multiply(x, 4.)
array([ 0.,  4.,  8.])
"""

import numpy as np
#cimport numpy as np

# See the Cython docs on declaring templated C++ classes:
# http://cython.readthedocs.io/en/latest/src/userguide/wrapping_CPlusPlus.html#templates
#
# See also, the definition of valarray in C++ standard library:
# http://en.cppreference.com/w/cpp/numeric/valarray
#
# We only declare here the methods we actually use
cdef extern from "<valarray>" namespace "std":
    cdef cppclass valarray[T]:
        valarray()
        valarray(int)  # constructor: empty constructor
        T& operator[](int)  # get/set element

def multiply(double[:] x, double factor):
    """Multiply the array x by some factor using c++ for some reason"""

    cdef valarray[double] v
    cdef int i
    cdef double[:] out_view  # memory view of output array

    # allocate C++ valarray
    cdef int nv = len(x)
    v = valarray[double](nv)
    cdef double* vview = &v[0]
    for i in range(len(x)):
        print i
        print x[i]
        vview[i] = x[i]
        print v[i]

    # multiply each element in the C++ valarray
    for i in range(len(x)):
        v[i] = v[i] * factor

    # allocate output numpy array.
    out = np.empty(len(x), dtype=np.double)
    out_view = out

    # copy C++ valarray to output numpy array
    for i in range(len(x)):
        out_view[i] = v[i]

    return out
