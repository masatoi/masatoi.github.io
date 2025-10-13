"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.EmojiRouter = void 0;
const express_1 = require("express");
const redirect = (req, res) => {
    const cdnUrl = "https://cdn.qiita.com";
    res.redirect(`${cdnUrl}${req.baseUrl}${req.path}`);
};
exports.EmojiRouter = (0, express_1.Router)().get("/:q*", redirect);
//# sourceMappingURL=emoji.js.map