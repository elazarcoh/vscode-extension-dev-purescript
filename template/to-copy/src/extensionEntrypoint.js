"use strict";
exports.__esModule = true;
exports.activate = void 0;
exports.deactivate = void 0;
function activate(context) {
    return require("./Extension").activateImpl(context)();
}
function deactivate() {
    return require("./Extension").deactivateImpl()();
}
exports.activate = activate;
exports.deactivate = deactivate;
