.PHONY: clean install

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

lint:
	yarn run lint

clean:
	rm -rf build
	rm package-lock.json || true
	rm yarn.lock || true

upgrade: clean install
	yarn upgrade next@latest react@latest react-bootstrap@latest react-dom@latest bootstrap@latest \
	     bootstrap-icons@latest eslint@latest eslint-config-next@latest @reduxjs/toolkit@latest \
	     next-redux-wrapper@latest react-redux@latest @popperjs/core@latest \
	     jest@latest jest-environment-jsdom@latest @testing-library/react@latest @testing-library/jest-dom@latest