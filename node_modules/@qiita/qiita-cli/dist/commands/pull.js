"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.pull = void 0;
const arg_1 = __importDefault(require("arg"));
const get_file_system_repo_1 = require("../lib/get-file-system-repo");
const get_qiita_api_instance_1 = require("../lib/get-qiita-api-instance");
const sync_articles_from_qiita_1 = require("../lib/sync-articles-from-qiita");
const pull = async (argv) => {
    const args = (0, arg_1.default)({
        "--force": Boolean,
        "-f": "--force",
    }, { argv });
    const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    const forceUpdate = args["--force"];
    await (0, sync_articles_from_qiita_1.syncArticlesFromQiita)({ fileSystemRepo, qiitaApi, forceUpdate });
    console.log("Sync local articles from Qiita");
    console.log("Successful!");
};
exports.pull = pull;
//# sourceMappingURL=pull.js.map