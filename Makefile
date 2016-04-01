prf: *hs
	ghc -o prf -O2 --make *hs
prf-debug: *hs
	ghc -o prf-debug -O2 --make *hs -rtsopts -prof -auto-all -caf-all -fforce-recomp

