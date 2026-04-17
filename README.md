# deepershallowsyntax README

This is a vscode extension which uses the ideas behind deeper shallow embeddings to automatically
derive a parser and typechecker for a given language from typing rules.

The backend is written in ocaml in src/extensionOcamlComponent, which is a dune project.
Build the ocaml code into javascript by runing "dune build" in that folder.
(You don't need dune/ocaml installed to run the extension, only to develop)

To run the VSCode extension:
run `npm install`, go to extension.ts, and press Ctrl-F5 (or just F5 if you want it to run extra slowly, in debugger mode).

If that works, it should open a new vscode window with the extension running. Within this new window, you can use the typechecking system.
To use it: write the spec file with a name "<lang-name>.language".
write a file in the language as "<name>.<lang-name>"
For both the spec files and language files, run the "Check Program" command in vscode, by pressing Ctrl-Shift-P and searching for "Check Program".
If there are any errors, they should be underlined red in the file.

The vscode window should open into the langtest folder, which has a bunch of examples already written.