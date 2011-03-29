#!/bin/bash
lein clean
rm -rf .sevenri/ lib/
find src/sevenri -name '*.class' -delete
for d in "documenter" "incantea" "planter"
do
    pushd src/project/slix/$d >/dev/null
    lein clean
    rm -rf lib/
	popd >/dev/null
done
