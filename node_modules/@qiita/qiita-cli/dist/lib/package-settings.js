"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.PackageSettings = void 0;
const packageJsonData = require("../../package.json");
exports.PackageSettings = {
    name: packageJsonData.name,
    userAgentName: "QiitaCLI",
    version: packageJsonData.version,
};
//# sourceMappingURL=package-settings.js.map