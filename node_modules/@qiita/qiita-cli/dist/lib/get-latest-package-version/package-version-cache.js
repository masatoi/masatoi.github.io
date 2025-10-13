"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.setCacheData = exports.getCacheData = void 0;
const node_fs_1 = __importDefault(require("node:fs"));
const node_path_1 = __importDefault(require("node:path"));
const config_1 = require("../config");
const CACHE_FILE_NAME = "latest-package-version";
const cacheFilePath = () => {
    const cacheDir = config_1.config.getCacheDataDir();
    return node_path_1.default.join(cacheDir, CACHE_FILE_NAME);
};
const getCacheData = () => {
    const filePath = cacheFilePath();
    if (!node_fs_1.default.existsSync(filePath)) {
        return null;
    }
    const data = node_fs_1.default.readFileSync(filePath, { encoding: "utf-8" });
    return JSON.parse(data);
};
exports.getCacheData = getCacheData;
const setCacheData = (data) => {
    const cacheDir = config_1.config.getCacheDataDir();
    const filePath = cacheFilePath();
    node_fs_1.default.mkdirSync(cacheDir, { recursive: true });
    node_fs_1.default.writeFileSync(filePath, JSON.stringify(data), {
        encoding: "utf-8",
        mode: 0o600,
    });
};
exports.setCacheData = setCacheData;
//# sourceMappingURL=package-version-cache.js.map