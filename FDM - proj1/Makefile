all: precise4 precise8 precise16

precise4:
	ifort -std08 -fpp -D"prec=4" -o "prec4" gaussian_elimination.f90 main.f90 

precise8:
	ifort -std08 -fpp -D"prec=8" -o "prec8" gaussian_elimination.f90 main.f90 

precise16:
	ifort -std08 -fpp -D"prec=16" -o "prec16" gaussian_elimination.f90 main.f90 
