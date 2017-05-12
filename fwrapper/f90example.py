import numpy as np
from os import path, system

print('Compiling Fortran extension.')
system(path.expanduser('f2py -c -m fits fitsmodule.f90'))
from fits import multiply

x = np.array([0., 1., 2.])
new = multiply(x, 4.)
print(new)

