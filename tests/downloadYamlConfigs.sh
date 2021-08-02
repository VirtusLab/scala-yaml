#!/bin/bash

declare -a repositories=("https://github.com/kubernetes/examples" "https://github.com/kubernetes/minikube" "https://github.com/kubernetes/kubernetes")

rm -rf repositories
mkdir repositories
cd repositories

## now loop through the above array
for i in "${repositories[@]}"
do
   git clone "$i"
   # or do whatever with individual element of the array
done

find ./repositories -name '*.yaml' -exec cp -prv '{}' './test-suite/jvm/src/it/resources/yaml/configs/' ';'
