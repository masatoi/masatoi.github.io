"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.preview = void 0;
const config_1 = require("../lib/config");
const get_file_system_repo_1 = require("../lib/get-file-system-repo");
const get_qiita_api_instance_1 = require("../lib/get-qiita-api-instance");
const getUrlAddress_1 = require("../lib/getUrlAddress");
const sync_articles_from_qiita_1 = require("../lib/sync-articles-from-qiita");
const app_1 = require("../server/app");
const preview = async () => {
    const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    await (0, sync_articles_from_qiita_1.syncArticlesFromQiita)({ fileSystemRepo, qiitaApi });
    const server = await (0, app_1.startServer)();
    const address = server.address();
    const url = (0, getUrlAddress_1.getUrlAddress)(address);
    if (url) {
        const open = (await import("open")).default;
        await open(url);
    }
    (0, app_1.startLocalChangeWatcher)({
        server,
        watchPath: config_1.config.getItemsRootDir(),
    });
};
exports.preview = preview;
//# sourceMappingURL=preview.js.map