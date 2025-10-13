"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.AssetsRouter = void 0;
const express_1 = require("express");
const get_qiita_api_instance_1 = require("../../lib/get-qiita-api-instance");
const redirectToArticleCss = async (req, res) => {
    const url = await resolveAssetsUrl("article_css_url");
    res.redirect(url);
};
const redirectToEmbedInitJs = async (req, res) => {
    const url = await resolveAssetsUrl("v3_embed_init_js_url");
    res.redirect(url);
};
const redirectToFavicon = async (req, res) => {
    const url = await resolveAssetsUrl("favicon_url");
    res.redirect(url);
};
const resolveAssetsUrl = (() => {
    let cachedAssetUrls = null;
    return async (key) => {
        if (cachedAssetUrls === null) {
            const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
            const assetUrls = await qiitaApi.getAssetUrls();
            cachedAssetUrls = assetUrls;
        }
        const url = cachedAssetUrls[key];
        if (!url) {
            throw new Error(`Asset not found: ${key}`);
        }
        return url;
    };
})();
exports.AssetsRouter = (0, express_1.Router)()
    .get("/article.css", redirectToArticleCss)
    .get("/embed-init.js", redirectToEmbedInitJs)
    .get("/favicon.ico", redirectToFavicon);
//# sourceMappingURL=assets.js.map