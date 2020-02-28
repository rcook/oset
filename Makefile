.PHONY: all
all:
	haskell-ci oset.cabal > .travis.yml
	stack --stack-yaml=stack-lts-15.1.yaml test --fast
	stack --stack-yaml=stack-lts-13.3.yaml test --fast
	stack --stack-yaml=stack-lts-12.6.yaml test --fast
	stack --stack-yaml=stack-lts-11.22.yaml test --fast
	stack --stack-yaml=stack-lts-9.21.yaml test --fast
