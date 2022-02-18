import * as vscode from 'vscode';

export const disposeImpl = (disposable: vscode.Disposable) => { return disposable.dispose(); };
export const subscribeDisposable =
    (typeClassDict) =>
        (disposable: vscode.Disposable) =>
            (ctx: vscode.ExtensionContext) =>
                () => {
                    ctx.subscriptions.push(disposable);
                }
