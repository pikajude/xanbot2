const path = require('path');
const UglifyJsPlugin = require('uglifyjs-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
// const CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = (env, argv) => {
  const prod = argv.mode === 'production';

  const dev = (p1, p2) => (argv.mode === 'production' ? p2 : p1);

  return {
    optimization: {
      minimizer: dev(
        [],
        [
          new UglifyJsPlugin({
            cache: true,
            parallel: true,
            sourceMap: true,
          }),
          new OptimizeCSSAssetsPlugin({}),
        ]
      ),
    },
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
                localIdentName: dev(
                  '[name]_[local]__[hash:base64:8]',
                  '[hash:base64:8]'
                ),
              },
            },
          ],
        },
        {
          test: /\.(woff|woff2|eot|ttf|svg)$/,
          loader: 'file-loader',
          options: {
            name: dev(
              'fonts/[name].[ext]',
              'fonts/[hash:base64:8].[ext]'
            ),
          },
        },
      ],
    },
    plugins: [
      new MiniCssExtractPlugin({ filename: '[name].css' }),
      // new CopyWebpackPlugin([{ from: 'icon', to: 'icon' }]),
    ],
  };
};
