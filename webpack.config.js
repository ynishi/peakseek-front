const Dotenv = require('dotenv-webpack');
const path = require('path');
const webpack = require('webpack');
const TerserPlugin = require('terser-webpack-plugin');

module.exports = (env, argv) => {
    var optimization = {};
    var devServer = {};
    var optimization = {};
    if (argv.mode === 'development') {
        optionsDebug = true;
        devServer = {
            inline: true,
            stats: 'errors-only'
        };
    }
    if (argv.mode === 'production') {
        optionsDebug = false;
        optimization = {
            minimizer: [
                new TerserPlugin({
                    parallel: true,
                    sourceMap: false,
                    terserOptions: {
                        compress: {
                            pure_funcs: [
                                'F2',
                                'F3',
                                'F4',
                                'F5',
                                'F6',
                                'F7',
                                'F8',
                                'F9',
                                'A2',
                                'A3',
                                'A4',
                                'A5',
                                'A6',
                                'A7',
                                'A8',
                                'A9'
                            ],
                            pure_getters: true,
                            keep_fargs: false,
                            unsafe_comps: true,
                            unsafe: true
                        },
                    }
                }),
                new TerserPlugin({
                    terserOptions: {
                        mangle: true,
                    }
                })
            ],
            splitChunks: {
                cacheGroups: {
                    default: false,
                    elm: {
                        test: /[\\/]Main.elm/,
                        name: 'elm',
                        chunks: 'all'
                    }
                }
            }
        };
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
                        debug: optionsDebug,
                        optimize: true,
                    }
                }
            ],
        },
        optimization: optimization,
        devServer: devServer
    }
};
