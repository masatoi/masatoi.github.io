"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getFileSystemRepo = void 0;
const config_1 = require("./config");
const file_system_repo_1 = require("./file-system-repo");
const getFileSystemRepo = async () => await file_system_repo_1.FileSystemRepo.build({
    dataRootDir: config_1.config.getItemsRootDir(),
});
exports.getFileSystemRepo = getFileSystemRepo;
//# sourceMappingURL=get-file-system-repo.js.map