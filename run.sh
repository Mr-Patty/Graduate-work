#!/bin/bash
stack build
# ./a.out
# | read par
# echo $par
var1=0
# for (( a = 0; a < 10; a++ ))
# do
#   var2=0
#   stdbuf -oL stack exec GraduateWork-exe |
#     while IFS= read -r line
#     do
#       let "var2 += line"
#       echo "$line"
#     done
# done
# let "var1 /= 10"
# # let var1=$(($var1 / 10))
# echo "$var1"

set +m # disable job control in order to allow lastpipe
shopt -s lastpipe
for (( a = 0; a < 100; a++ ))
do
  stack exec GraduateWork-exe |
    while IFS= read -r line; do lines[i]=$(echo "scale=9; $line" | bc); ((i++)); done
  var1+=$(echo "scale=9; ${lines[0]}" | bc)
  # let "var1 += ${lines[0]}"
  echo "${lines[0]}"
  echo "$var1"
done
echo "$var1"
# interest_r=$(echo "scale=9; $var1/100.0" | bc)
# echo "$interest_r"
# let "var2 = var1 / 100"
# let "var3 = var1 % 100"
# echo "${lines[0]}" # second line
# echo "$var2"
# echo "$var3"
