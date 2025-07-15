.PHONY: clean install build serve dev lint format upgrade update-current swizzled-components-upgrade

package-lock.json:
	npm install

install: package-lock.json

build: package-lock.json
	npm run build

serve: build
	npm run serve

serve-static: build
	if [ -d "serve_static/" ]; then rm -rf serve_static/; fi
	mkdir serve_static
	mv build/ serve_static/doc

	@echo "=========================================================================="
	@echo "\033[1mServing static production build at: \033[34m http://localhost:3002/doc \033[0m"
	python3 -m http.server --bind 127.0.0.1 3002 --directory serve_static

dev:
	npm run start

clean:
	rm -rf build
	rm package-lock.json || true

lint:
	npm run pretty:check

format:
	npm run pretty:write

upgrade: clean install
	# Update to the latest version of react and react-dom when it is supported and does not create dependency conflicts
	npm install \
	@docusaurus/core@latest \
	@docusaurus/plugin-client-redirects@latest \
	@docusaurus/plugin-sitemap@latest \
	@docusaurus/preset-classic@latest \
	@docusaurus/theme-mermaid@latest \
	clsx@latest \
	react@^18.3.1 \
	react-dom@^18.3.1 \
	redocusaurus@latest

update-current:
	# Use docusaurus command to create a new version using a temporary name
	npm run docusaurus docs:version updated
	# cleanup previous file & replace it with the updated ones
	rm -rf versioned_docs/version-maintained versioned_sidebars/version-maintained-sidebars.json
	mv versioned_docs/version-updated versioned_docs/version-maintained
	mv versioned_sidebars/version-updated-sidebars.json  versioned_sidebars/version-maintained-sidebars.json
	# Remove the entry for the temporary version in the versions.json file
	sed -i '/updated/d' versions.json

swizzled-components-upgrade:
	@echo "\033[1mRemoving swizzled components\033[0m"
	rm -rf ./src/theme/Footer ./src/theme/Navbar

	@echo "\033[1mRe-swizzling Docusaurus components\033[0m"
	npm run swizzle @docusaurus/theme-classic Footer/Layout -- --eject --typescript
	npm run swizzle @docusaurus/theme-classic Footer/LinkItem -- --eject --typescript
	npm run swizzle @docusaurus/theme-classic Footer/Links/MultiColumn -- --eject --typescript
	npm run swizzle @docusaurus/theme-classic Footer/Logo -- --eject --typescript

	npm run swizzle @docusaurus/theme-classic Navbar/Content -- --eject --typescript --danger
	npm run swizzle @docusaurus/theme-classic Navbar/Layout -- --eject --typescript --danger
	npm run swizzle @docusaurus/theme-classic Navbar/MobileSidebar/Layout -- --eject --typescript --danger
	npm run swizzle @docusaurus/theme-classic Navbar/MobileSidebar/PrimaryMenu -- --eject --typescript --danger

	@echo "\033[1mApplying prettier\033[0m"
	prettier --write ./src/theme/Footer ./src/theme/Navbar

	@echo "\033[1mTrying to apply custom changes ...\033[0m conflicts must be handled manually"
	git apply ./upgrade/*.patch
