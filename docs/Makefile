.PHONY: clean install

yarn.lock:
	yarn install

install: yarn.lock

build: yarn.lock
	yarn && yarn run build

serve: build
	yarn run serve

dev:
	yarn run start

clean:
	rm -rf build
	rm package-lock.json || true
	rm yarn.lock || true

upgrade: clean install
	yarn upgrade @docusaurus/core@latest @docusaurus/preset-classic@latest redocusaurus@latest