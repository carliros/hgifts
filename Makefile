
build:
	spago build
	npm run tw:build
	spago bundle-app -t ./web/assets/js/app/app.js

watch:
	spago build
	npm run tw:build
	spago bundle-app --watch -t ./web/assets/js/app/app.js
