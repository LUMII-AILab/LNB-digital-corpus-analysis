#!/bin/bash
ghc --make -O -feager-blackholing -threaded -rtsopts -fforce-recomp LNB_transform_source.hs
for ((i=243; i>=200; i=i-1)) 
#for ((i=0;i>=0; i=i-1))
do
	echo "Processing ${i}000 "`date`
	./LNB_transform_source ${i}000 1000 +RTS -N3
done 
