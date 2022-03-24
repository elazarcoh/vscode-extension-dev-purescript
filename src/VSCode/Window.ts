import * as vscode from "vscode";

export const _showInformationMessage = msg => items => () => vscode.window.showInformationMessage(msg, undefined, ...items);
export const _showErrorMessage = msg => items => () => vscode.window.showErrorMessage(msg, undefined, ...items);
export const _showWarningMessage = msg => items => () => vscode.window.showWarningMessage(msg, undefined, ...items);