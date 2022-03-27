// @ts-nocheck
const gulp = require('gulp');
const webpack = require('webpack-stream');
const ts = require('gulp-typescript');
const shell = require('gulp-shell');

const jsFiles = ['src/extensionEntrypoint.js', 'src/utils.js'];

gulp.task('purescript', shell.task('spago build'));
gulp.task('typescript', function () {
    var tsProject = ts.createProject('tsconfig.json');
    return tsProject.src()
        .pipe(tsProject())
        .pipe(
            gulp.dest(function (file) {
                return file.base;
            })
        );
});
gulp.task('copy-js', function () {
    return gulp.src(jsFiles).pipe(gulp.dest('output/'));
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

gulp.task('build-dev', gulp.series(['typescript', 'copy-js', 'purescript']));

gulp.task(
    'watch',
    gulp.series([
        'build-dev',
        function watch() {
            process.stdout.write('\033c');
            gulp.watch('./src/**/*.purs', gulp.series('purescript'));
            gulp.watch(
                './src/**/*.ts',
                gulp.series(['typescript', 'purescript'])
            );
            gulp.watch(jsFiles, gulp.series('copy-js'));
        },
    ])
);
gulp.task('default', gulp.series('watch'));
