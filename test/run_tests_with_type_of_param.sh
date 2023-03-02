if [[ -z $1 ]]; then
  echo "Please inform target test application"
fi
../obj/test/$1; exit $?