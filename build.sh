ghc --make Main.hs -o main
cat input.txt | ./main > output.txt
dot output.txt -Tpng -o tree.png
