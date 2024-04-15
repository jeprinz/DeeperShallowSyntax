# deepershallowsyntax README

This is a vscode extension which uses the ideas behind deeper shallow embeddings to automatically
derive a parser and typechecker for a given language from typing rules.

The backend is written in ocaml because I hope to reuse that code for other projects.
The ocaml code is in src/extensionOccamlComponent, which is a dune project.
Build the ocaml code into javascript by runing "dune build" in that folder.

To test the VSCode extension, go to extension.ts, and press F5.
Also: to run the javascript faster, use Ctrl-F5. F5 by default goes into the debugger, which seems to be
vastly slower.