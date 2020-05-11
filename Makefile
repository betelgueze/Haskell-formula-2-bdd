all:
	ghc formula-2-bdd.hs
    
zip:
	zip flp-fun-xrisam00.zip formula-2-bdd.hs Makefile README ./tests/* run-tests.sh
    
clean:
	rm -f formula-2-bdd.o formula-2-bdd formula-2-bdd.hi

test:
	ghc formula-2-bdd.hs
	./run-tests.sh
	rm -f formula-2-bdd.o formula-2-bdd formula-2-bdd.hi