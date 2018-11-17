module.exports = {
	"parser": "babel-eslint",
	"env": {
		"node": true
	},
	"plugins": ["flowtype", "prettier"],
	"extends": ["eslint:recommended", "prettier"],
	"parserOptions": {
	},
	"rules": {
		"no-console": "off",
		"no-constant-condition": "off",
		"strict": [
			"error",
			"global"
		],
		"no-var": "error",
		"prefer-const": "error",
		"prettier/prettier": "error"
	}
};
