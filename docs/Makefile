.PHONY: clean install build serve dev upgrade

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

update-current:
	# Use docusaurus command to create a new version using a temporary name
	yarn run docusaurus docs:version updated
	# cleanup previous file & replace it with the updated ones
	rm -rf versioned_docs/version-maintained versioned_sidebars/version-maintained-sidebars.json
	mv versioned_docs/version-updated versioned_docs/version-maintained
	mv versioned_sidebars/version-updated-sidebars.json  versioned_sidebars/version-maintained-sidebars.json
	# Remove the entry for the temporary version in the versions.json file
	sed -i '/updated/d' versions.json
