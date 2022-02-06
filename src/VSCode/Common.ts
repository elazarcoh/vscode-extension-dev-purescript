import * as vscode from 'vscode';

export const disposeImpl = (disposable: vscode.Disposable) => { return disposable.dispose(); };