.PHONY: clean install

yarn.lock:
	yarn install

install: yarn.lock

build: yarn.lock
	yarn && yarn run build

serve: build
	yarn run start

dev:
	yarn run dev

lint:
	yarn run lint

clean:
	rm -rf build
	rm package-lock.json || true
	rm yarn.lock || true

upgrade: clean install
	yarn upgrade