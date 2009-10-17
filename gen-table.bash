#/bin/bash
echo "removing old classfiles"
rm classes/*
echo "generating new classfiles"
echo "(compile 'lojten)" | clj -cp classes
echo "generating table"
java -cp ./clojure.jar:./classes lojten --gen > lojten.mim
