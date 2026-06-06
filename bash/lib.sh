#!/usr/bin/env bash

# Useful helpers, mostly lifted from ‘direnv stdlib’
___has() {
  type "$1" &>/dev/null
}
___PATH_add() {
  ___path_add PATH "$@"
}
___path_add() {
  local path i var_name="$1"
  declare -a path_array
  IFS=: read -ra path_array <<<"${!1-}"
  shift
  for ((i = $#; i > 0; i--)); do
    path_array=("$(___expand_path "${!i}")" ${path_array[@]+"${path_array[@]}"})
  done
  path=$(
    IFS=:
    echo "${path_array[*]}"
  )
  export "$var_name=$path"
}
___expand_path() {
  local REPLY; ___realpath.absolute "${2+"$2"}" "${1+"$1"}"; echo "$REPLY"
}
___realpath.dirname() { REPLY=.; ! [[ $1 =~ /+[^/]+/*$|^//$ ]] || REPLY="${1%${BASH_REMATCH[0]}}"; REPLY=${REPLY:-/}; }
___realpath.basename(){ REPLY=/; ! [[ $1 =~ /*([^/]+)/*$ ]] || REPLY="${BASH_REMATCH[1]}"; }
___realpath.absolute() {
  REPLY=$PWD; local eg=extglob; ! shopt -q $eg || eg=; ${eg:+shopt -s $eg}
  while (($#)); do case $1 in
    //|//[^/]*) REPLY=//; set -- "${1:2}" "${@:2}" ;;
    /*) REPLY=/; set -- "${1##+(/)}" "${@:2}" ;;
    */*) set -- "${1%%/*}" "${1##${1%%/*}+(/)}" "${@:2}" ;;
    ''|.) shift ;;
    ..) ___realpath.dirname "$REPLY"; shift ;;
    *) REPLY="${REPLY%/}/$1"; shift ;;
  esac; done; ${eg:+shopt -u $eg}
}
___path_add_if_exists() {
    local var_name="${1:?}"; shift
    local dirs=()
    for dir in "$@"; do
        if [[ -d $dir ]]; then
            dirs+=("$dir")
        fi
    done
    ___path_add "$var_name" "${dirs[@]}"
}

# Helper to keep only the first occurence of each component of a
# colon-separated path string.
_deduplicate_path_components() {
    echo "$1" | awk -v RS=: -v ORS=: '!a[$1]++' | sed -e 's/:$//'
}

# Find the closest parent directory that contains a descendant.
# Starts from the current working directory unless overridden.
_closest_parent_dir_containing() {
    local descendant="${1:?}"
    local dir="${2:-$(pwd)}"
    while true; do
        if [[ -e $dir/$descendant ]]; then
            echo "$dir"
            return 0
        elif [[ $dir = '/' ]]; then
            return 1;
        fi
        dir="$(dirname "$dir")"
    done
}
