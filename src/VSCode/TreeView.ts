import * as vscode from 'vscode';
import { TreeItem } from 'vscode';


export const _registerTreeView =
    (id: string) =>
        (getChildren: <T>(element: T | undefined) => T[]) =>
            (getTreeItem: <T>(element: T | undefined) => TreeItem) => {
                return () => {
                    class TreeDataProvider implements vscode.TreeDataProvider<TreeItem> {
                        constructor() { }
                        getChildren = getChildren;
                        getTreeItem = getTreeItem;
                    }
                    const disposable = vscode.window.registerTreeDataProvider(id, new TreeDataProvider());
                    return disposable;
                };
            };
;
