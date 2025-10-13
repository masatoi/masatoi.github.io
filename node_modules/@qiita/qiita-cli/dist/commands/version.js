"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.version = void 0;
const package_settings_1 = require("../lib/package-settings");
const version = async () => {
    console.log(package_settings_1.PackageSettings.version);
};
exports.version = version;
//# sourceMappingURL=version.js.map