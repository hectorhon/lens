module.exports = {
  env: {
    commonjs: true,
    es6: true,
    node: true,
  },
  extends: [
    'airbnb-base',
  ],
  globals: {
    Atomics: 'readonly',
    SharedArrayBuffer: 'readonly',
  },
  parserOptions: {
    ecmaVersion: 2018,
  },
  rules: {
    'semi': ['error', 'never'],
    'comma-dangle': 'off',
    'arrow-parens': ['error', 'as-needed'],
  },
  settings: {
    'import/resolver': {
      webpack: {
        'config-index': 1 // update if webpack.config.js changes. Currently points to django
      }
    }
  }
};
