"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.packageUpdateNotice = void 0;
const get_latest_package_version_1 = require("./get-latest-package-version");
const package_settings_1 = require("./package-settings");
const packageUpdateNotice = async () => {
    const currentVersion = package_settings_1.PackageSettings.version;
    const latestVersion = await (0, get_latest_package_version_1.getLatestPackageVersion)();
    if (!latestVersion) {
        return null;
    }
    if (currentVersion === latestVersion) {
        return null;
    }
    const chalk = (await import("chalk")).default; // `chalk` supports only ESM.
    const boxen = (await import("boxen")).default; // `boxen` supports only ESM.
    let message = "新しいバージョンがあります! ";
    message += ` ${chalk.red(currentVersion)} -> ${chalk.green(latestVersion)}`;
    message += "\n";
    message += `${chalk.green(`npm install ${package_settings_1.PackageSettings.name}@latest`)}`;
    message += " でアップデートできます!";
    message = boxen(message, { padding: 1, margin: 1, borderStyle: "round" });
    return message;
};
exports.packageUpdateNotice = packageUpdateNotice;
//# sourceMappingURL=package-update-notice.js.map