stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack
package = test-proj
version = v0.0.1

run: build
	stack exec test

build:
	$(stack) build $(package)

ghci:
	$(stack) ghci $(package):lib

ghcid:
	ghcid -c "$(stack) ghci $(package):lib"
