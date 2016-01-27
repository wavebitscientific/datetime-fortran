#!/usr/bin/env bash
echo "Running mkdist.sh"
echo

# set some colour coding
# http://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`


# Extra checks on the integrity of packaging
echo "Checking versioning consistency"
vers_conf=$(grep AC_INIT configure.ac | sed -e 's/^.*\[//g' | sed -e 's/\])//')
echo "configure.ac reports version: '$vers_conf'"
vers_src=$(grep Version src/lib/datetime.f90 | sed -e 's/! Version: *//')
echo "datetime.f90 reports version: '$vers_src'"
if [[ "$vers_conf" == "$vers_src" ]]
then
    echo "${green}Pass${reset}"
    echo "Remember to tag the repo"
else
    echo "${red}FAIL${reset}: Versions do not match. Aborting"
    exit 1
fi
echo

#echo "Do 'make distcheck' for final sanity check"
