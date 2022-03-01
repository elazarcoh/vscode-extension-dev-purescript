import * as vscode from 'vscode';
import { curryThunk } from './utils';

export const _registerCommand = c => fn => () => vscode.commands.registerCommand(c, fn);
// export const _executeCommand = curryThunk(vscode.commands.executeCommand);
export const _getCommands = curryThunk(vscode.commands.getCommands);
// export const _getCommands = ()=>{ throw new Error('Not implemented');}