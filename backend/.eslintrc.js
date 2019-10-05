module.exports = {
  env: {
    browser: true,
    es6: true,
  },
  extends: [
    'airbnb',
  ],
  globals: {
    "$": true,
  },
  parserOptions: {
    ecmaVersion: 2018,
    sourceType: 'module',
  },
  rules: {
    'semi': ['error', 'never'],
    'comma-dangle': 'off',
    'arrow-parens': ['error', 'as-needed'],
    'space-before-function-paren': ['error', 'never'],
    'func-names': ['error', 'never'],
  },
}
