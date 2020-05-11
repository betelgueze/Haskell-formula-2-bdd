#!/bin/bash

green='\e[1;32m%s\e[0m\n'
red='\e[1;31m%s\e[0m\n'

mkdir -p tmp && rm -rf tmp/*

./formula-2-bdd -i "tests/example1.in" >tmp/"test01".in 2>/dev/null
./formula-2-bdd -t "tests/example1.in" >tmp/"test01".tab 2>/dev/null
./formula-2-bdd -b "tests/example1.in" >tmp/"test01".bdd 2>/dev/null
./formula-2-bdd -r "tests/example1.in" >tmp/"test01".rbdd 2>/dev/null

diff --strip-trailing-cr -u "tests/example1.in" "tmp/test01.in" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 01 - Identity PASS"
else
	printf "$red" "Test 01 - Identity %sFAIL"
fi

diff --strip-trailing-cr -u "tests/example1.tab" "tmp/test01.tab" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 02 - Truth table PASS"
else
	printf "$red" "Test 02 - Truth table FAIL"
fi

diff --strip-trailing-cr -u "tests/example1.bdd" "tmp/test01.bdd" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 03 - BDD PASS"
else
	printf "$red" "Test 03 - BDD FAIL"
fi

diff --strip-trailing-cr -u "tests/example1.rbdd" "tmp/test01.rbdd" >/dev/null 2>&1
if [ $? -eq 0 ]; then
	printf "$green" "Test 04 - Reduced BDD PASS"
else
	printf "$red" "Test 04 - Reduced BDD FAIL"
fi

#rm -rf tmp