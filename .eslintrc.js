module.exports = {
	"parser": "babel-eslint",
	"env": {
		"node": true
	},
	"plugins": ["flowtype", "prettier"],
	"extends": ["eslint:recommended", "prettier", "plugin:flowtype/recommended"],
	"parserOptions": {
	},
	"rules": {
		"flowtype/space-after-type-colon": ["error", "always", { "allowLineBreak": true }],
		"eqeqeq": ["error", "always", { "null": "ignore" }],
		"no-console": "warn",
		"no-constant-condition": "off",
		"no-param-reassign": "error",
		"sort-imports": ["error", { "ignoreCase": true }],
		"strict": [
			"error",
			"global"
		],
		"no-var": "error",
		"prefer-const": "error",
		"prettier/prettier": "error"
	}
};
