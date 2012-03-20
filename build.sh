ghc --make Main.hs -o main
./main input.txt output.txt
dot output.txt -Tpng -o tree.png
