const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = {
  mode: 'development',
  plugins: [
    new MiniCssExtractPlugin({
      filename: 'stylesheets/app/compiled/[name].css'
    })
  ],
  module: {
    rules: [
      {
        test: /\.jsx$/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-react']
          }
        }
      },
      {
        test: /\.scss$/,
        use: [{
          loader: MiniCssExtractPlugin.loader,
          options: {
            publicPath: path.resolve(__dirname, 'static/stylesheets/app/compiled/'),
          }
        }, {
          loader: 'css-loader'
        }, {
          loader: 'sass-loader'
        }]
      }
    ]
  },
  entry: {
    imageSelectTool: './widgets/images/select-tool/select-tool.jsx',
  },
  output: {
    filename: 'javascripts/app/compiled/[name].js',
    path: path.resolve(__dirname, 'static/'),
  }
};
