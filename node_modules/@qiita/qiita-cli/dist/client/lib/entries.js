"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.fromEntries = exports.entries = void 0;
// Wrapped Object.entries with strict type
const entries = (o) => {
    return Object.entries(o);
};
exports.entries = entries;
// Wrapped Object.fromEntries with strict type
const fromEntries = (e) => {
    return Object.fromEntries(e);
};
exports.fromEntries = fromEntries;
//# sourceMappingURL=entries.js.map