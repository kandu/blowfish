PROJECT= blowfish

.PHONY: build, install uninstall test clean

build:
	jbuilder build

install: build
	jbuilder install

uninstall: build
	jbuilder uninstall

test:
	jbuilder runtest

clean:
	jbuilder clean 
