"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __exportStar = (this && this.__exportStar) || function(m, exports) {
    for (var p in m) if (p !== "default" && !Object.prototype.hasOwnProperty.call(exports, p)) __createBinding(exports, m, p);
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.QiitaApi = void 0;
const node_url_1 = require("node:url");
const errors_1 = require("./errors");
const debugger_1 = require("./lib/debugger");
__exportStar(require("./errors"), exports);
class QiitaApi {
    constructor({ token, userAgent }) {
        this.token = token;
        this.userAgent = userAgent ? userAgent : QiitaApi.defaultUserAgent();
    }
    static defaultUserAgent() {
        return `${QiitaApi.agentName}/${QiitaApi.version}`;
    }
    getUrlScheme() {
        return "https";
    }
    getDomainName() {
        return process.env.QIITA_DOMAIN ? process.env.QIITA_DOMAIN : "qiita.com";
    }
    getBaseUrl() {
        const hostname = this.getDomainName();
        return `${this.getUrlScheme()}://${hostname}/`;
    }
    getPreviewUrl() {
        return `${this.getUrlScheme()}://${this.getDomainName()}`;
    }
    async request(url, options) {
        let response;
        try {
            (0, debugger_1.qiitaApiDebugger)(`request to`, url, JSON.stringify(options));
            response = await fetch(url, {
                headers: {
                    Authorization: `Bearer ${this.token}`,
                    "Content-Type": "application/json",
                    "User-Agent": this.userAgent,
                },
                ...options,
            });
        }
        catch (err) {
            console.error(err);
            throw new errors_1.QiitaFetchError(err.message);
        }
        if (response.ok) {
            const body = await response.text();
            try {
                return JSON.parse(body);
            }
            catch {
                return body;
            }
        }
        const responseBody = await response.text();
        if (debugger_1.qiitaApiDebugger.enabled) {
            (0, debugger_1.qiitaApiDebugger)("request failed", JSON.stringify({
                status: response.status,
                responseBody,
            }));
        }
        const errorMessage = responseBody.slice(0, 100);
        switch (response.status) {
            case 400:
                throw new errors_1.QiitaBadRequestError(errorMessage);
            case 401:
                throw new errors_1.QiitaUnauthorizedError(errorMessage);
            case 403:
                throw new errors_1.QiitaForbiddenError(errorMessage);
            case 404:
                throw new errors_1.QiitaNotFoundError(errorMessage);
            case 429:
                throw new errors_1.QiitaRateLimitError(errorMessage);
            case 500:
                throw new errors_1.QiitaInternalServerError(errorMessage);
            default:
                throw new errors_1.QiitaUnknownError(errorMessage);
        }
    }
    generateApiUrl(path) {
        const baseUrl = path === "/api/preview" ? this.getPreviewUrl() : this.getBaseUrl();
        return new node_url_1.URL(path, baseUrl).toString();
    }
    async get(path, options) {
        const url = this.generateApiUrl(path);
        return await this.request(url, {
            ...options,
            method: "GET",
        });
    }
    async post(path, options) {
        const url = this.generateApiUrl(path);
        return await this.request(url, {
            ...options,
            method: "POST",
        });
    }
    async patch(path, options) {
        const url = this.generateApiUrl(path);
        return await this.request(url, {
            ...options,
            method: "PATCH",
        });
    }
    async authenticatedUser() {
        return await this.get("/api/v2/authenticated_user");
    }
    async authenticatedUserItems(page, per) {
        const params = new node_url_1.URLSearchParams();
        if (page !== undefined) {
            params.set("page", page.toString());
        }
        if (per !== undefined) {
            params.set("per_page", per.toString());
        }
        const path = `/api/v2/authenticated_user/items?${params}`;
        return await this.get(path);
    }
    async preview(rawBody) {
        const body = JSON.stringify({
            parser_type: "qiita_cli",
            raw_body: rawBody,
        });
        return await this.post("/api/preview", {
            body,
        });
    }
    async items(page, per, query) {
        const params = new node_url_1.URLSearchParams();
        if (page !== undefined) {
            params.set("page", page.toString());
        }
        if (per !== undefined) {
            params.set("per_page", per.toString());
        }
        if (query !== undefined) {
            params.set("query", query);
        }
        const path = `/api/v2/items?${params}`;
        return await this.get(path);
    }
    async postItem({ rawBody, tags, title, isPrivate, organizationUrlName, slide, }) {
        const data = JSON.stringify({
            body: rawBody,
            title,
            tags: tags.map((name) => {
                return {
                    name,
                    versions: [],
                };
            }),
            private: isPrivate,
            organization_url_name: organizationUrlName,
            slide,
        });
        const path = `/api/v2/items`;
        return await this.post(path, {
            body: data,
        });
    }
    async patchItem({ uuid, rawBody, title, tags, isPrivate, organizationUrlName, slide, }) {
        const data = JSON.stringify({
            body: rawBody,
            title,
            tags: tags.map((name) => {
                return {
                    name,
                    versions: [],
                };
            }),
            private: isPrivate,
            organization_url_name: organizationUrlName,
            slide,
        });
        const path = `/api/v2/items/${uuid}`;
        return await this.patch(path, {
            body: data,
        });
    }
    async getAssetUrls() {
        return await this.get("/api/qiita-cli/assets");
    }
}
exports.QiitaApi = QiitaApi;
QiitaApi.agentName = "QiitaApi";
QiitaApi.version = "0.0.1";
//# sourceMappingURL=index.js.map