"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.itemUrl = void 0;
const node_process_1 = __importDefault(require("node:process"));
const node_url_1 = require("node:url");
const getDomainName = () => {
    if (node_process_1.default.env.QIITA_DOMAIN) {
        return node_process_1.default.env.QIITA_DOMAIN;
    }
    return "qiita.com";
};
const getBaseUrl = () => {
    const domainName = getDomainName();
    return `https://${domainName}/`;
};
const itemUrl = ({ id, userId, secret = false, }) => {
    const baseUrl = getBaseUrl();
    const subdir = secret ? "private" : "items";
    const path = `/${userId}/${subdir}/${id}`;
    return new node_url_1.URL(path, baseUrl).toString();
};
exports.itemUrl = itemUrl;
//# sourceMappingURL=qiita-url.js.map