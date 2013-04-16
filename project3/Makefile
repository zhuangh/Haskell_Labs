SOEPATH=/Users/zhuangh/Codes/cse230/SOE/src

run: all
	./Main

clean:
	rm *.o *.hi Main


all:
	# ghc -Wall -i${SOEPATH} -i${PARSECPATH} -v --make Main 
	ghc -i${SOEPATH} -hide-package parsec-3.1.3 --make Main
	# -hide-package parsec-3.1.3 --make Main 
	#ghc -fno-warn-name-shadowing -i${SOEPATH} --make Main
