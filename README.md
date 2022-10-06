This branch is used for testing LFortran.

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
