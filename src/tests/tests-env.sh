#!/usr/bin/env bash

# remove escape codes

OS=$(uname -s)
if [ "X$OS" = "XDarwin" ]; then
    noesc () { sed 's/\[[0-9]*m*//g' ; }
else
    # http://www.commandlinefu.com/commands/view/3584/remove-color-codes-special-characters-with-sed
    noesc () { sed -r 's/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g' ; }
fi

LOG=tests-env.sh.log
./datetime_tests | noesc > $LOG

TRS=tests-env.sh.trs

# extract lines that end in PASS or FAIL
pof () { awk '/PASS$/ {print $0} ; /FAIL$/ {print $0}' ; }

# back to front. Replace the first word by the last
btf () { awk '{$1=$NF;}1'; }

rmres () { sed 's/: PASS$//' | sed 's/: FAIL$//' ; }

pof <$LOG | btf |  rmres  | awk '{print ":test-result: " $0}' >$TRS

