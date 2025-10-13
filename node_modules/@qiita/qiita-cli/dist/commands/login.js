"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.login = void 0;
const node_process_1 = __importDefault(require("node:process"));
const promises_1 = __importDefault(require("node:readline/promises"));
const config_1 = require("../lib/config");
const get_qiita_api_instance_1 = require("../lib/get-qiita-api-instance");
const login = async () => {
    const rl = promises_1.default.createInterface({
        input: node_process_1.default.stdin,
        output: node_process_1.default.stdout,
    });
    const chalk = (await import("chalk")).default;
    console.log(`
以下のURLにアクセスしてトークンを発行してください。（「read_qiita」と「write_qiita」にチェックを入れてください）
  ${chalk.bold("https://qiita.com/settings/tokens/new?read_qiita=1&write_qiita=1&description=qiita-cli")}
  `);
    const token = await rl.question("発行したトークンを入力: ");
    rl.close();
    const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)({ token });
    const currentUser = await qiitaApi.authenticatedUser();
    await config_1.config.setCredential({
        name: "qiita",
        accessToken: token,
    });
    console.log(`Hi ${currentUser.id}!\n`);
    await printAvailableCommands();
};
exports.login = login;
const printAvailableCommands = async () => {
    const chalk = (await import("chalk")).default;
    console.log(`ログインが完了しました 🎉
以下のコマンドを使って執筆を始めましょう！

🚀 コンテンツをブラウザでプレビューする
  ${chalk.bold("npx qiita preview")}

🚀 新しい記事を追加する
  ${chalk.bold("npx qiita new (記事のファイルのベース名)")}

🚀 記事を投稿、更新する
  ${chalk.bold("npx qiita publish (記事のファイルのベース名)")}

💁 コマンドのヘルプを確認する
  ${chalk.bold("npx qiita help")}
  `);
};
//# sourceMappingURL=login.js.map