"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.escape = void 0;
const REGEXP = /[&'`"<>]/g;
const map = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#x27;",
    "`": "&#x60;",
};
const replace = (match) => map[match];
const escape = (html) => html.replace(REGEXP, replace); // eslint-disable-line @typescript-eslint/no-explicit-any
exports.escape = escape;
//# sourceMappingURL=escape.js.map