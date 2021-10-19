
build:
	spago build
	spago bundle-app -t ./web/assets/js/app/app.js

watch:
	spago build
	spago bundle-app --watch -t ./web/assets/js/app/app.js
