"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getLatestPackageVersion = void 0;
const fetch_package_version_1 = require("./fetch-package-version");
const package_version_cache_1 = require("./package-version-cache");
const CACHE_EXPIRE_TIME = 1000 * 60 * 60 * 12; // 12 hours
const getLatestPackageVersion = async () => {
    const cacheData = (0, package_version_cache_1.getCacheData)();
    const now = Date.now();
    if (cacheData) {
        const { lastCheckedAt, latestVersion } = cacheData;
        if (now - lastCheckedAt < CACHE_EXPIRE_TIME) {
            return latestVersion;
        }
    }
    const latestVersion = await (0, fetch_package_version_1.fetchLatestPackageVersion)();
    if (latestVersion) {
        (0, package_version_cache_1.setCacheData)({
            lastCheckedAt: now,
            latestVersion,
        });
    }
    return latestVersion;
};
exports.getLatestPackageVersion = getLatestPackageVersion;
//# sourceMappingURL=get-latest-package-version.js.map