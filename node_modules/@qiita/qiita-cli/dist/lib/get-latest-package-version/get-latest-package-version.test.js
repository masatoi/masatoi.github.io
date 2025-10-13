"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
const fetchPackageVersion = __importStar(require("./fetch-package-version"));
const get_latest_package_version_1 = require("./get-latest-package-version");
const packageVersionCache = __importStar(require("./package-version-cache"));
describe("getLatestPackageVersion", () => {
    const mockFetchLatestPackageVersion = jest.spyOn(fetchPackageVersion, "fetchLatestPackageVersion");
    const mockGetCacheData = jest.spyOn(packageVersionCache, "getCacheData");
    const mockSetCacheData = jest.spyOn(packageVersionCache, "setCacheData");
    const mockDateNow = jest.spyOn(Date, "now");
    beforeEach(() => {
        mockFetchLatestPackageVersion.mockReset();
        mockGetCacheData.mockReset();
        mockSetCacheData.mockReset();
    });
    describe("when cache exists and not expired", () => {
        const cacheData = {
            lastCheckedAt: new Date("2023-07-13T00:00:00.000Z").getTime(),
            latestVersion: "0.0.0",
        };
        beforeEach(() => {
            mockGetCacheData.mockReturnValue(cacheData);
            mockDateNow.mockReturnValue(new Date("2023-07-13T11:00:00.000Z").getTime());
        });
        it("returns cached version", async () => {
            expect(await (0, get_latest_package_version_1.getLatestPackageVersion)()).toEqual("0.0.0");
            expect(mockGetCacheData).toHaveBeenCalled();
            expect(mockFetchLatestPackageVersion).not.toHaveBeenCalled();
            expect(mockDateNow).toHaveBeenCalled();
            expect(mockSetCacheData).not.toHaveBeenCalled();
        });
    });
    describe("when cache exists but expired", () => {
        const cacheData = {
            lastCheckedAt: new Date("2023-07-13T00:00:00.000Z").getTime(),
            latestVersion: "0.0.0",
        };
        const currentTime = new Date("2023-07-13T12:00:00.000Z").getTime();
        beforeEach(() => {
            mockGetCacheData.mockReturnValue(cacheData);
            mockDateNow.mockReturnValue(currentTime);
            mockFetchLatestPackageVersion.mockResolvedValue("0.0.1");
            mockSetCacheData.mockReturnValue();
        });
        it("returns latest version and updates cache", async () => {
            expect(await (0, get_latest_package_version_1.getLatestPackageVersion)()).toEqual("0.0.1");
            expect(mockGetCacheData).toBeCalled();
            expect(mockDateNow).toHaveBeenCalled();
            expect(mockFetchLatestPackageVersion).toHaveBeenCalled();
            expect(mockSetCacheData).toHaveBeenCalledWith({
                lastCheckedAt: currentTime,
                latestVersion: "0.0.1",
            });
        });
    });
    describe("when cache does not exist", () => {
        const currentTime = new Date("2023-07-13T12:00:00.000Z").getTime();
        beforeEach(() => {
            mockGetCacheData.mockReturnValue(null);
            mockDateNow.mockReturnValue(currentTime);
            mockFetchLatestPackageVersion.mockResolvedValue("0.0.1");
            mockSetCacheData.mockReturnValue();
        });
        it("returns latest version and updates cache", async () => {
            expect(await (0, get_latest_package_version_1.getLatestPackageVersion)()).toEqual("0.0.1");
            expect(mockGetCacheData).toBeCalled();
            expect(mockDateNow).toHaveBeenCalled();
            expect(mockFetchLatestPackageVersion).toHaveBeenCalled();
            expect(mockSetCacheData).toHaveBeenCalledWith({
                lastCheckedAt: currentTime,
                latestVersion: "0.0.1",
            });
        });
    });
});
//# sourceMappingURL=get-latest-package-version.test.js.map