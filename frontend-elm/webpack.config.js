const path = require('path')
const CleanWebpackPlugin = require('clean-webpack-plugin');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const SriPlugin = require('webpack-subresource-integrity');
const CopyWebpackPlugin = require('copy-webpack-plugin');

const { versionIdFromEnv } = require('./webpack/helper')

const distPath = path.join(__dirname,
  'dist/adminonly-assets')
  // '..', 'frontend/app/adminonly-assets')

const versionId = versionIdFromEnv()

var config = (prodMode) => {
  return {

    entry: {
      app: './src/adminonly.js'
    },

    output: {
      path: distPath,
      filename: `adminonly-elm-${versionId}.js`,
      publicPath: '/'
    },

    plugins: [
      new SriPlugin({
        // FIXME fix WARNING output due to cross-origin policy
        // FIXME make sure we have CSP Headers with `require-sri-for script style`
        enabled: prodMode,
        hashFuncNames: ['sha256']
      }),

      // Only used in prodMode
      new MiniCssExtractPlugin({
        filename: `[name]-${versionId}.css`,
        chunkFilename: `[id]-${versionId}.css`
      }),

      new CopyWebpackPlugin([{
        from: "assets",
        to: distPath
      }])
    ],

    module: {
      rules: [
        {
          test: /\.html$/,
          use: {
            loader: 'html-loader',
            options: {
              minimize: prodMode
            }
          }
        },
        {
          test: /\.(png|svg|jpg)$/,
          use: {
            loader: 'file-loader',
            options: {
                name: prodMode ? '[hash].[ext]' : '[name].[ext]',
                outputPath: distPath + 'images/'
            }
          }
        },
        {
          test: /\.(sa|sc|c)ss$/,
          use: [
            MiniCssExtractPlugin.loader,
            'css-loader',
            { loader: 'postcss-loader',
              options: {
                plugins: function () {
                  return [
                    require('precss'),
                    require('autoprefixer'),
                    prodMode ? require('cssnano') : () => {}
                  ];
                }
              }
            },
            { loader: 'sass-loader' },
          ]
        },
        { // NOTE:
          // Currently does not optimize the Elm generated JS using uglify, see:
          // https://elm-lang.org/0.19.0/optimize, although currently this makes
          // too little difference for us to care, re-evaluate later...
          // TODO add an elm-format lint test somewhere, doesn't need to be here
          // TODO allow the generation of elm documentation, does it look OK?
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            'elm-webpack-loader?verbose=true' +
                (prodMode ? '&optimize=true' : '&debug=true')
          ]
        }
      ]
    }
  };
};

module.exports = (env, argv) => {
  return config(argv.mode == 'production');
};
