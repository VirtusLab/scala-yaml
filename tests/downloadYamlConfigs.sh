#!/bin/bash

declare -a repositories=("https://github.com/kubernetes/examples" "https://github.com/kubernetes/minikube" "https://github.com/kubernetes/kubernetes")

mkdir repositories
cd repositories

## now loop through the above array
for i in "${repositories[@]}"
do
   git clone "$i"
   exit_code=$?
   repo_name="${i##*/}"
   echo $repo_name

   if [ $exit_code -eq 128 ]; then
     cd $repo_name
     git pull
     cd ..
   else
     git clone "$i"
   fi

   # or do whatever with individual element of the array
done

cd ..
find ./repositories -name '*.yaml' -exec cp -prv '{}' './test-suite/jvm/src/it/resources/yaml/configs/' ';'

LIB_YAML_PATH=""

for f in ./test-suite/jvm/src/it/resources/yaml/configs/*.yaml; do
    cat $f | $LIB_YAML_PATH >/dev/null

    ret=$?
    if [ $ret -ne 0 ]; then
      rm $f
    fi

done