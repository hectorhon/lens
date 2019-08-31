const path = require('path')

module.exports = {
  entry: {
    'hello-world': './widgets/hello-world.jsx'
  },
  output: {
    filename: '[name].js',
    path: path.join(__dirname, '/public/javascripts/dist/')
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
