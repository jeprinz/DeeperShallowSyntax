// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

import * as test from './test';

// import * as backend from './extensionOcamlComponent/_build/default/bin/main.bc.js';
// import * as backend2 from './extensionOcamlComponent/_build/default/bin/showtest';
import * as backend from './extensionOcamlComponent/_build/default/bin/main.bc';

import * as path from 'path';

// The error recieved back from the ocaml code are of this form:
type errorMessage = {
	leftLineNumber : number,
	leftPosInLine : number,
	rightLineNumber : number,
	rightPosInLine : number,
	severity : string,
	message : string
}

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

	context.subscriptions.push(vscode.commands.registerCommand('deepershallowsyntax.checkProgram', () => {
		console.log("The current file name is:", vscode.window.activeTextEditor?.document.fileName);
		//@ts-ignore
		console.log("The current file extension is:", path.basename(vscode.window.activeTextEditor?.document.fileName));

		doCheck();
	}));

	const extension = "language";
	let diagnosticCollection = vscode.languages.createDiagnosticCollection(extension);
	context.subscriptions.push(diagnosticCollection);

	function setErrors(errors : Array<errorMessage>){
		diagnosticCollection.clear();
		let str = vscode.window.activeTextEditor?.document.fileName
		if (str){
			diagnosticCollection.set(vscode.Uri.file(str),
			errors.map((error) => 
				new vscode.Diagnostic(new vscode.Range(new vscode.Position(error.leftLineNumber, error.leftPosInLine),
					new vscode.Position(error.rightLineNumber,error.rightPosInLine)), error.message, vscode.DiagnosticSeverity.Error)
			));
		}
	}

	// This regex will match things of the form "____.XXXX.language"
	const progNameRegex = new RegExp('^[^.]+.([^.]+).' + extension + '$');
	// This regex will match things of the form "XXXX.language"
	const specNameRegex = new RegExp('^([^.]+).' + extension + '$');

	function doCheck(){
		// First, determine by the file extension whether this is a spec file or a language file

		let fullNameWithDir = (vscode.window.activeTextEditor?.document.fileName);
		if (!fullNameWithDir){
			vscode.window.showInformationMessage("Error: couldn't get file name");
		} else {
			let fileName = path.basename(fullNameWithDir);
			let specNameMatch = fileName.match(specNameRegex);
			let progNameMatch = fileName.match(progNameRegex);
			if (specNameMatch) {
				let name = specNameMatch[1];
				// This is a specification file with name <name>
				vscode.window.showInformationMessage('Spec file with extension: ' + name);

				let text = vscode.window.activeTextEditor?.document.getText();

				// @ts-ignore
				let errors = backend.backend.checkSpec(text);

				console.log("Recieved back:", errors);
				vscode.window.showInformationMessage("Recieved this many errors:" + errors.length);
				setErrors(errors);
			} else if (progNameMatch){
				let lang = progNameMatch[1];
				// This is a specification file with language <lang>
				let progText = vscode.window.activeTextEditor?.document.getText();
				let specPath = fullNameWithDir.slice(0, fullNameWithDir.length - fileName.length) + lang + "." + extension;
				console.log("fullNameWithDir is: " + fullNameWithDir + " and specPath is: " + specPath);
				vscode.workspace.openTextDocument(specPath).then((specDoc) => {
					let specText = specDoc.getText();
					//@ts-ignore
					let errors = backend.backend.checkProgram(specText, progText);
					setErrors(errors);
				});
				vscode.window.showInformationMessage('Prog file with extension: ' + lang);
			} else {
				vscode.window.showInformationMessage('Filename not of the right form');
			}
		}
	}
}

// This method is called when your extension is deactivated
export function deactivate() {}
