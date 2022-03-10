import * as vscode from "vscode";


export const _showInformationMessage = msg => items => () => vscode.window.showInformationMessage(msg, undefined, ...items);