const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');

module.exports = (env, argv) => {
  const prod = argv.mode === 'production';
  return {
    mode: 'development',
    entry: './flow/xanbot.js',
    output: {
      path: path.resolve(__dirname, 'static'),
      filename: '[name].js',
    },
    resolve: {
      modules: [
        'node_modules',
        path.resolve(__dirname, 'css'),
        path.resolve(__dirname, 'flow'),
      ],
    },
    module: {
      rules: [
        {
          test: /\.m?js$/,
          exclude: /(node_modules|bower_components)/,
          use: {
            loader: 'babel-loader',
            options: {
              presets: [
                '@babel/preset-env',
                '@babel/preset-flow',
                '@babel/preset-react',
              ],
              plugins: [
                '@babel/plugin-proposal-class-properties',
                '@babel/plugin-syntax-dynamic-import',
                ['@babel/plugin-transform-runtime', { regenerator: true }],
              ],
            },
          },
        },
        {
          test: /\.css$/,
          use: [
            MiniCssExtractPlugin.loader,
            {
              loader: 'css-loader',
              options: {
                modules: true,
                importLoaders: 1,
                localIdentName: `${
                  prod ? '' : '[name]_[local]__'
                }[hash:base64:5]`,
              },
            },
          ],
        },
        {
          test: /\.(woff|woff2|eot|ttf|svg)$/,
          loader: 'file-loader',
          options: {
            name: 'fonts/[hash:8].[ext]'
          }
        },
      ],
    },
    plugins: [new MiniCssExtractPlugin({ filename: '[name].css' })],
  };
};
