// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

import * as test from './test';

// import * as backend from './extensionOcamlComponent/_build/default/bin/main.bc.js';
// import * as backend2 from './extensionOcamlComponent/_build/default/bin/showtest';
import * as backend from './extensionOcamlComponent/_build/default/bin/main.bc';

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {

	// Use the console to output diagnostic information (console.log) and errors (console.error)
	// This line of code will only be executed once when your extension is activated
	console.log('Congratulations, your extension "deepershallowsyntax" is now active!');
	test.doStuffHere();
	console.log(backend);
	// @ts-ignore
	// console.log(backend.backend.printMessage(1));
	backend.backend.printMessage(1);

	// @ts-ignore
	// backend.printMessage(1);
	// @ts-ignore
	console.log("running from backend: " + backend.backend.test(5));

	console.log("got here");

	// The command has been defined in the package.json file
	// Now provide the implementation of the command with registerCommand
	// The commandId parameter must match the command field in package.json
	let disposable = vscode.commands.registerCommand('deepershallowsyntax.helloWorld', () => {
		// The code you place here will be executed every time your command is executed
		// Display a message box to the user
		vscode.window.showInformationMessage('Hello World from DeeperShallowSyntax!');
	});

	context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated
export function deactivate() {}
