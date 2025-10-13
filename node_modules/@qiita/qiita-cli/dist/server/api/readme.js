"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.ReadmeRouter = void 0;
const express_1 = require("express");
const promises_1 = __importDefault(require("node:fs/promises"));
const node_path_1 = __importDefault(require("node:path"));
const get_qiita_api_instance_1 = require("../../lib/get-qiita-api-instance");
const readmeIndex = async (req, res) => {
    try {
        const fileContent = await promises_1.default.readFile(node_path_1.default.join(node_path_1.default.resolve(__dirname), "../../../", "README.md"), { encoding: "utf8" });
        const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
        const renderedBody = await qiitaApi.preview(fileContent);
        res.json({
            renderedBody,
        });
    }
    catch (err) {
        res.status(404).json({
            message: "Not found",
        });
    }
};
exports.ReadmeRouter = (0, express_1.Router)().get("/", readmeIndex);
//# sourceMappingURL=readme.js.map