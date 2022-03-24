'use strict';
const webpack = require('webpack');

const path = require('path');

const isWatch = process.argv.includes('--watch');
const plugins = [];

module.exports = {
    devtool: 'eval-source-map',
    target: "node",

    entry: './src/extension.js',
    output: {
        path: path.resolve(__dirname, 'dist'),
        filename: 'extension.js',
        libraryTarget: 'commonjs2',
        devtoolModuleFilenameTemplate: '../[resource-path]',
    },

    module: {
        rules: [
            {
                test: /\.purs$/,
                use: [
                    {
                        loader: 'purs-loader',
                        options: {
                            spago: true,
                            bundle: false,
                            watch: isWatch,
                        },
                    },
                ],
            },
        ],
    },

    resolve: {
        modules: ['node_modules', 'bower_components'],
        extensions: ['.purs', '.js'],
    },

    externals: {
        vscode: 'commonjs vscode', // the vscode-module is created on-the-fly and must be excluded.
    },

    plugins: [
        new webpack.LoaderOptionsPlugin({
            debug: true,
        }),
    ].concat(plugins),
};
