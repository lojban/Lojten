#/bin/bash
echo "removing old classfiles, if any"
rm -r classes
mkdir classes
echo "generating new classfiles"
echo "(compile 'lojten)" | java -cp ./clojure.jar:./classes:. clojure.lang.Repl
echo "generating table"
java -cp ./clojure.jar:./classes lojten --gen > lojten.mim
echo "clearing up"
rm -r classes
