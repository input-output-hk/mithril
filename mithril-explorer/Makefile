.PHONY: clean install build dev test watch-test lint upgrade

yarn.lock:
	yarn install

install: yarn.lock

build: yarn.lock
	yarn && yarn run build

serve: build
	yarn run start

dev:
	@echo "Serving dev build at: http://localhost:3000/explorer"
	yarn run dev

test: yarn.lock
	yarn run test:ci

watch-test: yarn.lock
	yarn run test

lint:
	yarn run lint

clean:
	rm -rf build
	rm package-lock.json || true
	rm yarn.lock || true

upgrade: clean install
	yarn upgrade \
		@popperjs/core@latest \
		@reduxjs/toolkit@latest \
		bootstrap@latest \
		bootstrap-icons@latest \
		next@latest \
		react@latest \
		react-bootstrap@latest \
		react-dom@latest \
		react-redux@latest \
		@testing-library/jest-dom@latest \
		@testing-library/react@latest \
		eslint@latest \
		eslint-config-next@latest \
		jest@latest \
		jest-environment-jsdom@latest \
		next-router-mock@latest \
