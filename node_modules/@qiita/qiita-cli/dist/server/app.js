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
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.startServer = startServer;
exports.startLocalChangeWatcher = startLocalChangeWatcher;
const chokidar_1 = __importDefault(require("chokidar"));
const express_1 = __importDefault(require("express"));
const node_http_1 = require("node:http");
const node_path_1 = __importDefault(require("node:path"));
const ws_1 = __importStar(require("ws"));
const debugger_1 = require("../lib/debugger");
const assets_1 = require("./api/assets");
const emoji_1 = require("./api/emoji");
const items_1 = require("./api/items");
const readme_1 = require("./api/readme");
const config_1 = require("../lib/config");
const getUrlAddress_1 = require("../lib/getUrlAddress");
async function startServer() {
    const app = (0, express_1.default)();
    app.use(express_1.default.json());
    app.use((req, res, next) => {
        (0, debugger_1.serverDebugger)(req.method, req.url, JSON.stringify(req.body || {}));
        next();
    });
    app.use(express_1.default.static(node_path_1.default.join(__dirname, "../public")));
    app.use("/api/items", items_1.ItemsRouter);
    app.use("/api/readme", readme_1.ReadmeRouter);
    app.use("/assets", assets_1.AssetsRouter);
    app.use("/emoji", emoji_1.EmojiRouter);
    app.use("*", express_1.default.static(node_path_1.default.join(__dirname, "../public/index.html")));
    const server = (0, node_http_1.createServer)(app);
    const userConfig = await config_1.config.getUserConfig();
    const port = userConfig.port;
    const host = userConfig.host;
    return new Promise((resolve, reject) => {
        server
            .listen(port, host)
            .once("listening", () => {
            const address = server.address();
            const url = (0, getUrlAddress_1.getUrlAddress)(address);
            if (url) {
                console.log(`Preview: ${url}`);
            }
            resolve(server);
        })
            .once("error", (err) => {
            reject(err);
        });
    });
}
function startLocalChangeWatcher({ server, watchPath, }) {
    const wsServer = new ws_1.WebSocketServer({ server });
    const watcher = chokidar_1.default.watch(watchPath);
    watcher.on("change", () => {
        wsServer.clients.forEach((client) => {
            if (client.readyState === ws_1.default.OPEN) {
                client.send("local changed");
            }
        });
    });
}
//# sourceMappingURL=app.js.map