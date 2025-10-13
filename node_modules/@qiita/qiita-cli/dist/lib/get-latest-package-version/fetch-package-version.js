"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.fetchLatestPackageVersion = void 0;
const package_settings_1 = require("../package-settings");
const fetchLatestPackageVersion = async () => {
    try {
        const response = await fetch(`https://registry.npmjs.org/${package_settings_1.PackageSettings.name}/latest`);
        const json = await response.json();
        const latestVersion = json.version;
        return latestVersion;
    }
    catch {
        return null;
    }
};
exports.fetchLatestPackageVersion = fetchLatestPackageVersion;
//# sourceMappingURL=fetch-package-version.js.map