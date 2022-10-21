This branch is used for testing LFortran. It contains code that is copied from SciPy, so once we can compile it, we can compile Minpack in SciPy.

How to build with GFortran:
```
git clone https://github.com/certik/minpack.git
cd minpack
git checkout -t origin/scipy
FC=gfortran cmake .
make
ctest
```

How to build with LFortran:
```
git clone https://github.com/certik/minpack.git
cd minpack
git checkout -t origin/scipy
FC=lfortran cmake .
make
ctest
```
