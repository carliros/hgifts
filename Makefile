
build:
	spago build
	npm run tw:build
	spago bundle-app -t ./docs/assets/js/app/app.js

watch:
	spago build
	npm run tw:build
	spago bundle-app --watch -t ./docs/assets/js/app/app.js
