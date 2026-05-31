.PHONY: all help test test-unit test-cli test-cli-config-for-paths-validation bundle build clean docker-build docker-publish

VERSION ?= latest
IMAGE_NAME ?= fukamachi/mallet
LOCAL_IMAGE_NAME ?= mallet

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
	@echo "Docker:"
	@echo "  make docker-build  - Build the mallet Docker image"
	@echo "  make docker-publish - Push the mallet Docker image"
	@echo ""
	@echo "Other:"
	@echo "  make clean         - Clean compilation cache"
	@echo "  make help          - Show this help message"
	@echo ""

test: test-unit test-cli

test-unit:
	@echo "Running unit tests..."
	@qlot exec sbcl --noinform --non-interactive \
		--eval "(when (find-package '#:qlot/local-init/setup) (setf (symbol-value (find-symbol \"*PROJECT-ROOT*\" '#:qlot/local-init/setup)) #p\"$(CURDIR)/\"))" \
		--eval '(asdf:load-system :mallet/tests)' \
		--eval '(or (rove:run :mallet/tests) (uiop:quit -1))'

test-cli:
	@echo ""
	@echo "Running CLI integration tests..."
	@./tests/cli-integration-test.sh
	@./tests/cli-exit-code-test.sh
	@./tests/cli-config-validation-errors-test.sh
	@$(MAKE) --no-print-directory test-cli-config-for-paths-validation

test-cli-config-for-paths-validation:
	@echo ""
	@echo "Running CLI config :for-paths validation tests..."
	@work_dir=$$(mktemp -d); \
	trap 'rm -rf "$$work_dir"' EXIT; \
	config_file="$$work_dir/mallet-config.lisp"; \
	source_file="$$work_dir/source.lisp"; \
	printf '(defun clean () t)\n' > "$$source_file"; \
	for config_content in \
		'(:mallet-config (:for-paths :keyword (:enable :line-length)))' \
		'(:mallet-config (:for-paths ("ok" :keyword) (:enable :line-length)))'; do \
		printf '%s\n' "$$config_content" > "$$config_file"; \
		exit_code=0; \
		output=$$(./bin/mallet --config "$$config_file" "$$source_file" 2>&1) || exit_code=$$?; \
		if [ "$$exit_code" -ne 3 ]; then \
			echo "Expected exit 3 for invalid :for-paths config, got $$exit_code"; \
			echo "$$output"; \
			exit 1; \
		fi; \
		case "$$output" in \
			Error:*) ;; \
			*) echo "Expected output to begin with Error:"; echo "$$output"; exit 1 ;; \
		esac; \
		if echo "$$output" | grep -Eq 'Fatal error:|TRIVIAL-GLOB|::'; then \
			echo "Output leaked an internal error detail"; \
			echo "$$output"; \
			exit 1; \
		fi; \
	done

bundle:
	@qlot bundle --exclude mallet/tests

build:
	@sbcl --noinform --non-interactive \
		--load init.lisp --eval "(asdf:make :mallet/executable)"

docker-build:
	docker build -t $(LOCAL_IMAGE_NAME):$(VERSION) .

docker-publish: docker-build
	docker tag $(LOCAL_IMAGE_NAME):$(VERSION) $(IMAGE_NAME):$(VERSION)
	docker push $(IMAGE_NAME):$(VERSION)

clean:
	@echo "Cleaning compilation cache and build artifacts..."
	@rm -f mallet
	@find . -name "*.fasl" -type f -delete
	@echo "Clean complete"
