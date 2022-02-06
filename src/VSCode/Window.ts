import * as vscode from "vscode";
import { curry } from './utils';

export const showInformationMessage = curry(vscode.window.showInformationMessage);