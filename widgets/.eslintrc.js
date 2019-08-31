module.exports = {
  env: {
    browser: true,
    es6: true,
  },
  extends: [
    'airbnb',
  ],
  globals: {
    Atomics: 'readonly',
    SharedArrayBuffer: 'readonly',
  },
  parserOptions: {
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 2018,
    sourceType: 'module',
  },
  plugins: [
    'react',
  ],
  rules: {
    'semi': ['error', 'never'],
    'comma-dangle': 'off',
    'arrow-parens': ['error', 'as-needed'],
    'react/jsx-indent-props': ['error', 'first'],
    'react/jsx-first-prop-new-line': ['error', 'never'],
    'react/jsx-closing-bracket-location': ['error', 'after-props'],
    'react/jsx-one-expression-per-line': 'off',
  },
}
