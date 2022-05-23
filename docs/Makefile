.PHONY: clean install

yarn.lock:
	yarn install

install: yarn.lock

build: yarn.lock
	yarn build

clean:
	rm -rf build
	rm package-lock.json || true
	rm yarn.lock || true

