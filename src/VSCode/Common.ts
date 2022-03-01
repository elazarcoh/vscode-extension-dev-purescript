import * as vscode from 'vscode';

export const disposeImpl = (disposable: vscode.Disposable) => { return disposable.dispose(); };
export const subscribeDisposable =
    (typeClassDict) =>
        (ctx: vscode.ExtensionContext) =>
            (disposable: vscode.Disposable) =>
                () => {
                    ctx.subscriptions.push(disposable);
                }
