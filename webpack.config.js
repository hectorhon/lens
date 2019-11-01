const path = require('path')

module.exports = [{
  name: 'widgets-workbench',
  entry: {
    'hello-world': './widgets/hello-world.jsx',
    'select-tool': './widgets/select-tool/index.jsx',
    'image-gallery': './widgets/image-gallery/index.jsx',
    table: './widgets/table/index.jsx',
  },
  output: {
    filename: '[name].js',
    path: path.join(__dirname, '/public/javascripts/dist/')
  },
  resolve: {
    extensions: ['.js', '.jsx']
  },
  module: {
    rules: [
      {
        test: /\.(js|jsx)$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }
      }
    ]
  }
}, {
  name: 'django',
  entry: {
    xxx: './backend/omrs/frontend/xxx.jsx',
  },
  output: {
    filename: '[name].js',
    path: path.join(__dirname, '/backend/backend/static/dist/'),
  },
  resolve: {
    extensions: ['.js', '.jsx'],
    alias: {
      Widgets: path.join(__dirname, '/widgets/'),
    }
  },
  module: {
    rules: [
      {
        test: /\.(js|jsx)$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader'
        }
      }
    ]
  }
}]
