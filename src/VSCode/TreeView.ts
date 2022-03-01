import { identity } from 'lodash';
import * as vscode from 'vscode';
import { TreeItem, TreeItemCollapsibleState } from 'vscode';


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
export const createTreeItemImpl = (
    { label, description, collapsibleState }: {
        label: string,
        description?: string,
        collapsibleState: TreeItemCollapsibleState
    }) => {
    const item = new TreeItem(label);
    item.description = description;
    item.collapsibleState = collapsibleState;
    return item
}