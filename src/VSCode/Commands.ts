import * as vscode from 'vscode';
import { curryThunk } from '../utils';

export const _registerCommand = c => fn => () => vscode.commands.registerCommand(c, fn);
export const _executeCommand = id => args => () => vscode.commands.executeCommand(id, ...args);
export const _getCommands = curryThunk(vscode.commands.getCommands);