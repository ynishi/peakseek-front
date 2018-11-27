const Dotenv = require('dotenv-webpack');
const path = require('path');
const webpack = require('webpack');

module.exports = (env, argv) => {
  if (argv.mode === 'development') {
    optionsDebug = true;
    devServer = {
      inline: true,
      stats: 'errors-only'
    };
  }
  if (argv.mode === 'production') {
    optionsDebug = false;
    devServer = {};
  }
  return {
    plugins: [
      new Dotenv({
        systemvars: true
      })
    ],
    module: {
      rules: [{
          test: /\.html$/,
          exclude: /node_modules/,
          loader: 'file-loader?name=[name].[ext]'
        },
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: "elm-webpack-loader",
          options: {
            debug: optionsDebug
          }
        }
      ],
    },
    devServer: devServer
  }
};
