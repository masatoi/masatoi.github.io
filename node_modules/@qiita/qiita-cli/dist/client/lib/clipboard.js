"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.writeClipboard = void 0;
const writeClipboard = (text) => {
    return navigator.clipboard.writeText(text);
};
exports.writeClipboard = writeClipboard;
//# sourceMappingURL=clipboard.js.map