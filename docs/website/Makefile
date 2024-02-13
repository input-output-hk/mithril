.PHONY: clean install build serve dev upgrade

package-lock.json:
	npm install

install: package-lock.json

build: package-lock.json
	npm run build

serve: build
	npm run serve

dev:
	npm run start

clean:
	rm -rf build
	rm package-lock.json || true

upgrade: clean install
	npm install \
	@docusaurus/core@latest \
	@docusaurus/preset-classic@latest \
	@docusaurus/theme-mermaid@latest \
	clsx@latest \
	react@latest \
	react-dom@latest \
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
