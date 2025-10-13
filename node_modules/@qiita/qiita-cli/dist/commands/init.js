"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.init = void 0;
const node_fs_1 = __importDefault(require("node:fs"));
const node_path_1 = __importDefault(require("node:path"));
const config_1 = require("../lib/config");
const publishWorkflowFileContent = `# Please set 'QIITA_TOKEN' secret to your repository
name: Publish articles

on:
  push:
    branches:
      - main
      - master
  workflow_dispatch:

permissions:
  contents: write

concurrency:
  group: \${{ github.workflow }}-\${{ github.ref }}
  cancel-in-progress: false

jobs:
  publish_articles:
    runs-on: ubuntu-latest
    timeout-minutes: 5
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - uses: increments/qiita-cli/actions/publish@v1
        with:
          qiita-token: \${{ secrets.QIITA_TOKEN }}
          root: "."
`;
const rootDir = process.cwd();
const workflowsDirectoryPath = node_path_1.default.join(rootDir, ".github/workflows");
const publishWorkflowFilePath = node_path_1.default.join(workflowsDirectoryPath, "publish.yml");
const gitignoreFilePath = node_path_1.default.join(rootDir, ".gitignore");
const gitignoreFileContent = `.remote
node_modules
`;
const init = async () => {
    console.log("設定ファイルを生成します。\n");
    if (!node_fs_1.default.existsSync(workflowsDirectoryPath)) {
        node_fs_1.default.mkdirSync(workflowsDirectoryPath, { recursive: true });
    }
    writeFile(publishWorkflowFilePath, publishWorkflowFileContent);
    writeFile(gitignoreFilePath, gitignoreFileContent);
    const userConfigFilePath = config_1.config.getUserConfigFilePath();
    const userConfigDir = config_1.config.getUserConfigDir();
    if (!node_fs_1.default.existsSync(userConfigFilePath)) {
        node_fs_1.default.mkdirSync(userConfigDir, { recursive: true });
    }
    const userConfigFileContent = JSON.stringify(await config_1.config.getUserConfig(), null, 2);
    writeFile(userConfigFilePath, userConfigFileContent);
    await printNextSteps();
};
exports.init = init;
const writeFile = (path, content) => {
    console.log(`  Creating ${path}`);
    if (!node_fs_1.default.existsSync(path)) {
        node_fs_1.default.writeFile(path, content, { encoding: "utf8" }, (err) => {
            if (err)
                throw err;
        });
        console.log(`     Created!\n`);
    }
    else {
        console.log(`     Already exists.\n`);
    }
};
const printNextSteps = async () => {
    const chalk = (await import("chalk")).default;
    console.log(`Success! ✨

次のステップ:

  1. トークンを作成してログインをしてください。
    ${chalk.bold("npx qiita login")}

  2. 記事のプレビューができるようになります。
    ${chalk.bold("npx qiita preview")}
  `);
};
//# sourceMappingURL=init.js.map