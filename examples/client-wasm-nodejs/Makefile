.PHONY: clean install upgrade

package-lock.json:
	npm install

install: package-lock.json

clean:
	rm -rf build
	rm package-lock.json || true

upgrade: clean install
	npm install \
		prettier@latest \


lint:
	npm run pretty:check

format:
	npm run pretty:write
