#!/usr/bin/env node
"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const arg_1 = __importDefault(require("arg"));
const dotenv_1 = __importDefault(require("dotenv"));
const commands_1 = require("./commands");
const config_1 = require("./lib/config");
const debugger_1 = require("./lib/debugger");
// TODO: Load `.env` file only in development environment.
dotenv_1.default.config();
const args = (0, arg_1.default)({
    "--credential": String,
    "--profile": String,
    "--config": String,
    "--root": String,
    "--verbose": Boolean,
}, {
    permissive: true,
});
const commandName = args._[0] || "preview";
const commandArgs = args._.slice(1);
if (args["--verbose"]) {
    (0, debugger_1.enableDebug)();
}
config_1.config.load({
    credentialDir: args["--credential"],
    profile: args["--profile"],
    itemsRootDir: args["--root"],
    userConfigDir: args["--config"],
});
(0, commands_1.exec)(commandName, commandArgs);
//# sourceMappingURL=main.js.map