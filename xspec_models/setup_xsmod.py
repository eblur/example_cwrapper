import numpy as np
from distutils.extension import Extension
from distutils.core import setup
from Cython.Build import cythonize


sources = ['calculate_flux_spectrum.pyx', 'minimal_powerlaw.cxx']

extension_obj_instance = Extension(name="calculate_flux_spectrum", sources=sources,
                                   include_dirs=[np.get_include()], language='c++')

setup(name="cython_wrapper", ext_modules=cythonize([extension_obj_instance]))
