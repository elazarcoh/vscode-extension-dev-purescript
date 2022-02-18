"use strict";
exports.__esModule = true;
exports.activate = void 0;
exports.deactivate = void 0;
function activate(context) {
    return require("./Main").activateImpl(context)();
}
function deactivate() {
    return require("./Main").deactivateImpl()();
}
exports.activate = activate;
exports.deactivate = deactivate;
