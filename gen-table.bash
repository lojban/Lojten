#/bin/bash
echo "removing old classfiles, if any"
rm -r classes
mkdir classes
echo "generating new classfiles"
echo "(compile 'lojten)" | clj -cp classes
echo "generating table"
java -cp ./clojure.jar:./classes lojten --gen > lojten.mim
echo "clearing up"
rm -r classes
