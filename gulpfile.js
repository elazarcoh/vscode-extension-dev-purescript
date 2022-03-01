// @ts-nocheck
const gulp = require('gulp');
const webpack = require('webpack-stream');
const ts = require('gulp-typescript');

gulp.task('typescript', function () {
    var tsProject = ts.createProject("tsconfig.json");
    return gulp
        .src('src/**/*.ts')
        .pipe(tsProject())
        .pipe(
            gulp.dest(function (file) {
                return file.base;
            })
        );
});

gulp.task('webpacker', function () {
    const config = {
        ...require('./webpack.config.js'),
        mode: 'development',
        optimization: {
            minimize: false,
            removeAvailableModules: false,
            removeEmptyChunks: false,
            splitChunks: false,
        },
    };

    return gulp
        .src('./src/Main.purs')
        .pipe(webpack(config))
        .pipe(gulp.dest('dist/'));
});

gulp.task('watch', function () {
    gulp.watch('./src/**/*.purs', gulp.series('webpacker'));
    gulp.watch('./src/**/*.ts', gulp.series(['typescript', 'webpacker']));
});

gulp.task('default', gulp.series(['typescript', 'webpacker', 'watch']));
