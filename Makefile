.PHONY: help test test-unit test-cli

help:
	@echo "Malo Linter - Make targets:"
	@echo ""
	@echo "  make test          - Run all tests (unit + CLI integration)"
	@echo "  make test-unit     - Run unit tests only"
	@echo "  make test-cli      - Run CLI integration tests only"
	@echo "  make install       - Install dependencies with Qlot"
	@echo "  make clean         - Clean compilation cache"
	@echo ""

test: test-unit test-cli

test-unit:
	@echo "Running unit tests..."
	@qlot exec sbcl --noinform --non-interactive \
		--eval "(asdf:test-system :malo)"

test-cli:
	@echo ""
	@echo "Running CLI integration tests..."
	@./tests/cli-integration-test.sh
