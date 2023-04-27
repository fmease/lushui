import { workspace, ExtensionContext } from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

const ID = 'lushui-language-server';
const NAME = 'Lushui Language Server';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
    client = createLanguageClient();
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    return client?.stop();
}

function createLanguageClient(): LanguageClient {
    const serverOptions: ServerOptions = {
        // @Task make this path customizable
        command: 'lushui-nightly',
        args: ['serve'],
        options: {
            env: {
                ...process.env,
                'LUSHUI_BACKTRACE': '1',
            },
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'lushui' }],
        synchronize: {
            fileEvents: [
                workspace.createFileSystemWatcher("**/package.recnot"),
            ],
        },
    };

    return new LanguageClient(
        ID,
        NAME,
        serverOptions,
        clientOptions
    );
}
