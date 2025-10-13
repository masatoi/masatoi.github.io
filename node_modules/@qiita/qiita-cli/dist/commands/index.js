"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.exec = void 0;
const error_handler_1 = require("../lib/error-handler");
const package_update_notice_1 = require("../lib/package-update-notice");
const help_1 = require("./help");
const init_1 = require("./init");
const login_1 = require("./login");
const newArticles_1 = require("./newArticles");
const preview_1 = require("./preview");
const publish_1 = require("./publish");
const pull_1 = require("./pull");
const version_1 = require("./version");
const exec = async (commandName, commandArgs) => {
    const commands = {
        init: init_1.init,
        login: login_1.login,
        new: newArticles_1.newArticles,
        preview: preview_1.preview,
        publish: publish_1.publish,
        pull: pull_1.pull,
        help: help_1.help,
        version: version_1.version,
        "--help": help_1.help,
        "--version": version_1.version,
    };
    const isCommand = (key) => {
        return commands.hasOwnProperty(key);
    };
    if (!isCommand(commandName)) {
        console.error(`Unknown command '${commandName}'`);
        console.error();
        console.error(help_1.helpText);
        process.exit(1);
    }
    const updateMessage = await (0, package_update_notice_1.packageUpdateNotice)();
    if (updateMessage) {
        console.log(updateMessage);
    }
    try {
        await commands[commandName](commandArgs);
    }
    catch (err) {
        console.error(err);
        await (0, error_handler_1.handleError)(err);
        process.exit(1);
    }
};
exports.exec = exec;
//# sourceMappingURL=index.js.map