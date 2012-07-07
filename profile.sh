#!/bin/bash
ghc --make -O -feager-blackholing -threaded -rtsopts -fforce-recomp -prof -caf-all -auto-all LNB_transform_source.hs
for ((i=2420; i>=2420; i=i-2)) 
do
	echo "Processing ${i}00 "`date`
	./LNB_transform_source ${i}00 50 +RTS -N4 -p -sstderr
done 
