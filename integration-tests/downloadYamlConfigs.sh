#!/bin/bash

declare -a repositories=(
  "https://github.com/kubernetes/examples"
  "https://github.com/kubernetes/minikube"
  "https://github.com/kubernetes/kubernetes"
  "https://github.com/openshift/source-to-image"
  "https://github.com/openshift/origin"
  )

gitDirectory="repositories"
mkdir "$gitDirectory"


# Download git repositories
for i in "${repositories[@]}"
do
   exit_code=$?
   repo_name="${i##*/}"
   git clone "$i" "$gitDirectory/$repo_name"

   if [ $exit_code -eq 128 ]; then
     # If repository is already downloaded, then only run git pull to update
     git pull "$gitDirectory/$repo_name"
   fi

done

mkdir ./test-suite/jvm/src/it/resources/yaml/configs/
find ./repositories -name '*.yaml' -exec cp -prv '{}' './test-suite/jvm/src/it/resources/yaml/configs/' ';'

LIB_YAML_PATH="" # Set the path to libyaml

# In downloaded repositories contains some invalid yaml, below instructions can remove this yaml
for f in ./test-suite/jvm/src/it/resources/yaml/configs/*.yaml; do
    cat $f | $LIB_YAML_PATH >/dev/null

    # if libyaml return error exit code, these means, that yaml is invalid
    exitCode=$?
    if [ $exitCode -ne 0 ]; then
      rm $f
    fi

done