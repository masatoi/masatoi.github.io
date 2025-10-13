"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.apiReadmeShowPath = exports.apiItemsUpdatePath = exports.apiItemsShowPath = exports.itemsShowPath = exports.itemsIndexPath = void 0;
const itemsIndexPath = () => {
    return "/";
};
exports.itemsIndexPath = itemsIndexPath;
const itemsShowPath = (itemId, params) => {
    const url = `/items/${itemId}`;
    if (itemId === "show" && params) {
        const query = new URLSearchParams(params).toString();
        return `${url}?${query}`;
    }
    else {
        return url;
    }
};
exports.itemsShowPath = itemsShowPath;
const apiItemsShowPath = (itemId, params) => {
    const url = `/api/items/${itemId}`;
    if (itemId === "show" && params) {
        const query = new URLSearchParams(params).toString();
        return `${url}?${query}`;
    }
    else {
        return url;
    }
};
exports.apiItemsShowPath = apiItemsShowPath;
const apiItemsUpdatePath = (itemId) => {
    return `/api/items/${itemId === "show" ? "post" : itemId}`;
};
exports.apiItemsUpdatePath = apiItemsUpdatePath;
const apiReadmeShowPath = () => {
    return "/api/readme";
};
exports.apiReadmeShowPath = apiReadmeShowPath;
//# sourceMappingURL=qiita-cli-url.js.map