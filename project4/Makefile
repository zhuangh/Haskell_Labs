UNAME=$(shell uname)

ifeq ($(UNAME), Linux)
  SOEPATH=/home/luzh/Haskell/SOE/src
  PARSEC_TO_HIDE=parsec-3.1.2
endif
ifeq ($(UNAME), Darwin)
  SOEPATH=/Users/zhangl/UCSD/CSE230-13W/SOE/src
  PARSEC_TO_HIDE=parsec-3.1.3
endif

WARNING_FLAG=-fno-warn-name-shadowing #-fno-warn-unused-do-bind #-fno-warn-name-shadowing # -Wall

run: all
	./Main

all:
	ghc ${WARNING_FLAG} -i${SOEPATH} -hide-package ${PARSEC_TO_HIDE} --make Main

clean:
	rm -rf *.o *.hi Main

ghci:
	ghci -i${SOEPATH} -hide-package ${PARSEC_TO_HIDE}

MAIL_TO=luzh@ucsd.edu
MAIL_SUBJ="Haskell Hw4"
MAIL_ATTH="./hw4.zip"
MAIL_MSG="This is my latest work of Haskell Hw4"

mail:
	zip hw4.zip hw4.lhs Main.hs Makefile
	echo ${MAIL_MSG} | mutt -a ${MAIL_ATTH} -s ${MAIL_SUBJ} -- ${MAIL_TO}
	rm -rf hw4.zip

# For ghci, after startup
# > :l hw4.lhs
# > import <something>
# > :set prompt "ghci hw4> "
