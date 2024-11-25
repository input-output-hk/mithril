.PHONY: clean install build dev test watch-test lint upgrade

package-lock.json:
	npm install

install: package-lock.json

build: package-lock.json
	npm run build
	# Fix to allow refresh & direct linking to the navigation page on static hosting such as Github Pages
	# this is because the nextJs router is only active for the home page at start.
	# A 'cleaner' way to do that must exist.
	mkdir out/registrations
	cp out/registrations.html out/registrations/index.html
	cp out/registrations.txt out/registrations/index.txt
	mkdir out/registrations-in-out
	cp out/registrations-in-out.html out/registrations-in-out/index.html
	cp out/registrations-in-out.txt out/registrations-in-out/index.txt

serve: build
	if [ -d "explorer/" ]; then rm -rf explorer/; fi
	mv out explorer/

	@echo "=========================================================================="
	@echo "\033[1mServing production build at: http://localhost:3001/explorer \033[0m"
	python3 -m http.server --bind 127.0.0.1 3001

dev: build
	@echo "=========================================================================="
	@echo "\033[1mServing dev build at: http://localhost:3000/explorer \033[0m"
	npm run dev

icons-font:
	npm run icons-font
	npm exec -- prettier ./src/app/mithril-icons.css --write

test: package-lock.json
	npm run test:ci

watch-test: package-lock.json
	npm run test

lint:
	npm run lint
	npm run pretty:check

format:
	npm run pretty:write

clean:
	rm -rf build
	rm package-lock.json || true

upgrade: clean install
	npm install \
		@popperjs/core@latest \
		@reduxjs/toolkit@latest \
		bootstrap@latest \
		bootstrap-icons@latest \
		chart.js@latest \
		next@latest \
		react@latest \
		react-bootstrap@latest \
		react-chartjs-2@latest \
		react-dom@latest \
		react-redux@latest \
		@testing-library/jest-dom@latest \
		@testing-library/react@latest \
		eslint@latest \
		eslint-config-next@latest \
		fantasticon@latest \
		jest@latest \
		jest-environment-jsdom@latest \
		next-router-mock@latest \
		prettier@latest \
		prettier-eslint@latest
