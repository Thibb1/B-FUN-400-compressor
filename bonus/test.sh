#!/bin/bash

compressor=${1:-./imageCompressor}

green() { printf "\033[32m${1}\033[0m\n"; }
orange() { printf "\033[38;5;208m${1}\033[0m\n"; }
blue() { printf "\033[38;5;27m${1}\033[0m\n" ; }
purpl() { printf "\033[38;5;92m${1}\033[0m\n" ; }

function run_test() {
    test_result=$($compressor $1 > /dev/null 2>&1)
    test_result=$?
    if [ $test_result -eq $2 ]; then
        green "✔ '${1}' -> ${test_result}"
    else
        orange "✖ '${1}' -> ${test_result} (expected: ${2})"
    fi
}

blue "Tests on argument management"

run_test "" 84
run_test "wrong_input" 84
run_test "-l -f" 84

run_test "-l 1 -f bonus/sample" 84
run_test "-l 1 -n 1" 84
run_test "-l 1 -n 1 -f" 84
run_test "-n 1 -f bonus/sample" 84
run_test "-l 1a -n 1 -f bonus/sample" 84
run_test "-l 1 -n 1a -f bonus/sample" 84
run_test "-l 1 -n 0 -f bonus/sample" 84
run_test "-l 1 -n 2 -f dontexist" 84

blue "Tests on image treatment"

run_test "-l 1 -n 2 -f bonus/sample" 0
run_test "-l 0.8 -n 2 -f bonus/sample" 0