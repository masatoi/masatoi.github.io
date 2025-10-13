"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.syncArticlesFromQiita = void 0;
const config_1 = require("./config");
const syncArticlesFromQiita = async ({ fileSystemRepo, qiitaApi, forceUpdate = false, }) => {
    const per = 100;
    const userConfig = await config_1.config.getUserConfig();
    for (let page = 1; page <= 100; page += 1) {
        const items = await qiitaApi.authenticatedUserItems(page, per);
        if (items.length <= 0) {
            break;
        }
        const result = userConfig.includePrivate
            ? items
            : items.filter((item) => !item.private);
        await fileSystemRepo.saveItems(result, forceUpdate);
    }
};
exports.syncArticlesFromQiita = syncArticlesFromQiita;
//# sourceMappingURL=sync-articles-from-qiita.js.map