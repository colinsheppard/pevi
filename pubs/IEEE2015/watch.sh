fswatch . -e ".*" -i ".tex" | xargs -n1 ./change.sh
