.PHONY: all help test test-unit test-cli bundle build clean

all: build

help:
	@echo "Malo Linter - Make targets:"
	@echo ""
	@echo "Build:"
	@echo "  make               - Build the malo executable (default)"
	@echo "  make build         - Build the malo executable"
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
		--eval '(asdf:load-system :malo/tests)' \
		--eval '(or (rove:run :malo/tests) (uiop:quit -1))'

test-cli:
	@echo ""
	@echo "Running CLI integration tests..."
	@./tests/cli-integration-test.sh

bundle:
	@qlot bundle --exclude malo/tests

build:
	@sbcl --noinform --non-interactive \
		--load init.lisp --eval "(asdf:make :malo)"

clean:
	@echo "Cleaning compilation cache and build artifacts..."
	@rm -f malo
	@find . -name "*.fasl" -type f -delete
	@echo "Clean complete"
