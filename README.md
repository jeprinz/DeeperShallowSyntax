# deepershallowsyntax README

This is a vscode extension which uses the ideas behind deeper shallow embeddings to automatically
derive a parser and typechecker for a given language from typing rules.

To run the VSCode extension, follow these very simple and easy steps:
- clone the repo
- run npm install
- go to src/extensionOcamlComponent (the interesting part of the code is written in ocaml, and compiled to javascript)
    - btw you need ocaml and dune installed.
    - run dune build in this folder
- open the main folder in vscode
- open src/extension.ts
- press Ctrl-F5 (I assume its Cmd-F5 on mac) (or just F5 if you want it to run extra slowly, in debugger mode).

If that works, it should open a new vscode window with the extension running. Within this new window, you can use the typechecking system.
To use it: write the spec file with a name "<lang-name>.language".
write a file in the language as "<name>.<lang-name>"
For both the spec files and language files, run the "Check Program" command in vscode, by pressing Ctrl-Shift-P and searching for "Check Program".
If there are any errors, they should be underlined red in the file.

There are a bunch of examples of language files, and files written in those languages, in the langtest folder.