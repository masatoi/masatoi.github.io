"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getQiitaApiInstance = void 0;
const qiita_api_1 = require("../qiita-api");
const config_1 = require("./config");
const package_settings_1 = require("./package-settings");
let qiitaApi;
const getQiitaApiInstance = async (options) => {
    if (!qiitaApi) {
        qiitaApi = new qiita_api_1.QiitaApi({
            token: await accessToken(options),
            userAgent: userAgent(),
        });
    }
    return qiitaApi;
};
exports.getQiitaApiInstance = getQiitaApiInstance;
const accessToken = async (options) => options?.token ?? (await config_1.config.getCredential()).accessToken;
const userAgent = () => {
    return `${package_settings_1.PackageSettings.userAgentName}/${package_settings_1.PackageSettings.version}`;
};
//# sourceMappingURL=get-qiita-api-instance.js.map