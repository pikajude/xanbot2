const path = require('path');

module.exports = {
  mode: 'development',
  entry:  './flow/xanbot.js',
  output: {
    path: path.resolve(__dirname, 'js'),
    filename: 'all.js',
  },
  module: {
    rules: [
      {
        test: /\.m?js$/,
        exclude: /(node_modules|bower_components)/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: ['@babel/preset-env', '@babel/preset-flow', '@babel/preset-react'],
            plugins: [
              '@babel/plugin-proposal-class-properties', 
              ['@babel/plugin-transform-runtime', { "regenerator": true }]
            ]
          }
        }
      },
      {
        test: /\.css$/,
        loader: 'style-loader!css-loader?modules&importLoaders=1&localIdentName=[name]__[local]___[hash:base64:5]' 
      }
    ]
  }
}
