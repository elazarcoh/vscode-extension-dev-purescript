import { identity } from 'lodash';
import * as vscode from 'vscode';
import { TreeItem } from 'vscode';
import { traced } from './utils';


export const _registerTreeView = (id: string) =>
    (getTreeItems: (element: TreeItem | undefined) => TreeItem[]) => {
        return () => {
            class TreeDataProvider implements vscode.TreeDataProvider<TreeItem> {
                constructor() { }
                getChildren = getTreeItems;
                getTreeItem = identity;
            }
            const disposable = vscode.window.registerTreeDataProvider(id, new TreeDataProvider());
            return disposable;
        };
    };
