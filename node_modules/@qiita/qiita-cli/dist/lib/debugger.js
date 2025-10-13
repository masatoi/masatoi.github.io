"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.serverDebugger = exports.configDebugger = exports.enableDebug = void 0;
const debug_1 = __importDefault(require("debug"));
const rootNameSpace = "qiita-cli";
const qiitaApiNameSpace = "qiita-api";
const enableDebug = () => {
    debug_1.default.enable(`${rootNameSpace}:*,${qiitaApiNameSpace}`);
};
exports.enableDebug = enableDebug;
exports.configDebugger = (0, debug_1.default)(`${rootNameSpace}:config`);
exports.serverDebugger = (0, debug_1.default)(`${rootNameSpace}:server`);
//# sourceMappingURL=debugger.js.map