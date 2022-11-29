'use strict';

// The module 'vscode' contains the VS Code extensibility API
import * as vscode from 'vscode';

import {
	window,
	workspace
} from 'vscode';
import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient';

let client: LanguageClient;

const outputChannel = window.createOutputChannel("Inferno LSP");

// * -= The Entry Point =- *
export async function activate(context: vscode.ExtensionContext) {

	console.log('..Extension "vscode-inferno-lsp-server" is now active..');

	const config = workspace.getConfiguration("vscode-inferno-lsp-server");

	const userDefinedExecutablePath = config.executable;

	let executablePath =  (userDefinedExecutablePath === '') ? 'inferno-lsp-server' : userDefinedExecutablePath;

	let serverOptions: ServerOptions = {
		run: { command: executablePath,
			   transport: TransportKind.stdio,
			   args: []
			 },
		debug: {
			command: executablePath,
			transport: TransportKind.stdio,
			args: []
		}
	};

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'inferno' }],
		synchronize: {
			configurationSection: 'vscode-inferno-lsp-server',
			// Notify the server about file changes to '.clientrc files contained in the workspace
			fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
		},
    initializationOptions: {
			'vscode-inferno-lsp-server': workspace.getConfiguration("vscode-inferno-lsp-server")
		},
		outputChannel: outputChannel
	};

	client = new LanguageClient(
		'vscode-inferno-lsp-server',
		'VSCode Inferno Language Server',
		serverOptions,
		clientOptions
	);

	client.start();
	outputChannel.appendLine("..Inferno LSP Server has been started..");
}

export function deactivate() {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

