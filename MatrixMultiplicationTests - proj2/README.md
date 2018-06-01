# Matrix Multiplication testing

Second Project for Fortran course.

## Getting ready

All you need to compile examples is: gfortran and pFUnit

### Main Example

Just type:

```
make test
./test
```

### opt2

Optimazation with DOT and CACHE

Type:

```
make tests
```

And then pick one of:

```
./test_normal
./test_dot
./test_cache
./test_both
```

### opt3

Measuring time of execution before and after optimalization.

Type:

```
make all
```

And then pick one of:

```
./normal N
./dot N
./cache N
./both N 
./matmul N //built-in function
```
