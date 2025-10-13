"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.newArticles = void 0;
const arg_1 = __importDefault(require("arg"));
const get_file_system_repo_1 = require("../lib/get-file-system-repo");
const newArticles = async (argv) => {
    const args = (0, arg_1.default)({}, { argv });
    const fileSystemRepo = await (0, get_file_system_repo_1.getFileSystemRepo)();
    if (args._.length > 0) {
        for (const basename of args._) {
            const createdFileName = await fileSystemRepo.createItem(basename);
            if (createdFileName) {
                console.log(`created: ${createdFileName}.md`);
            }
            else {
                console.error(`Error: '${basename}.md' is already exist`);
            }
        }
    }
    else {
        const createdFileName = await fileSystemRepo.createItem();
        if (createdFileName) {
            console.log(`created: ${createdFileName}.md`);
        }
        else {
            console.error("Error: failed to create");
        }
    }
};
exports.newArticles = newArticles;
//# sourceMappingURL=newArticles.js.map