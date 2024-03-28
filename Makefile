VERSION = 3.6.0

default :: lib

lib ::
	cabal configure
	cabal build
	cabal haddock

install-user ::
	cabal install --user

install-global ::
	sudo cabal install --global

# build :: dist-newstyle/build/libHShsshellscript-$(VERSION).a

# dist-newstyle/build/libHShsshellscript-$(VERSION).a :: 
# 	cabal build

dist ::
	cabal sdist

install-manual ::
	sudo mkdir -p /usr/local/share/hsshellscript/manual
	sudo cp -rv manual/* /usr/local/share/hsshellscript/manual
	sudo rm -f /usr/local/share/hsshellscript/manual/*~

uninstall-manual ::
	sudo rm -rf /usr/local/share/hsshellscript/manual
	sudo rmdir --ignore-fail-on-non-empty /usr/local/share/hsshellscript 

doc ::
	cabal haddock
