import * as vscode from "vscode";
import { inputTraced } from "../utils";

export const _showInformationMessage = msg => () => vscode.window.showInformationMessage(msg);