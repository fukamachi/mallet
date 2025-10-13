.PHONY: all help test test-unit test-cli bundle build clean

all: build

help:
	@echo "Mallet Linter - Make targets:"
	@echo ""
	@echo "Build:"
	@echo "  make               - Build the mallet executable (default)"
	@echo "  make build         - Build the mallet executable"
	@echo "  make bundle        - Bundle dependencies for standalone distribution"
	@echo ""
	@echo "Testing:"
	@echo "  make test          - Run all tests (unit + CLI integration)"
	@echo "  make test-unit     - Run unit tests only"
	@echo "  make test-cli      - Run CLI integration tests only"
	@echo ""
	@echo "Other:"
	@echo "  make clean         - Clean compilation cache"
	@echo "  make help          - Show this help message"
	@echo ""

test: test-unit test-cli

test-unit:
	@echo "Running unit tests..."
	@qlot exec sbcl --noinform --non-interactive \
		--eval '(asdf:load-system :mallet/tests)' \
		--eval '(or (rove:run :mallet/tests) (uiop:quit -1))'

test-cli:
	@echo ""
	@echo "Running CLI integration tests..."
	@./tests/cli-integration-test.sh

bundle:
	@qlot bundle --exclude mallet/tests

build:
	@sbcl --noinform --non-interactive \
		--load init.lisp --eval "(asdf:make :mallet)"

clean:
	@echo "Cleaning compilation cache and build artifacts..."
	@rm -f mallet
	@find . -name "*.fasl" -type f -delete
	@echo "Clean complete"
