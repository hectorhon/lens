const path = require('path')

module.exports = {
  entry: {
    'hello-world': './widgets/hello-world.jsx',
    'select-tool': './widgets/select-tool/index.jsx'
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
}
