"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getCurrentUser = void 0;
const get_qiita_api_instance_1 = require("../../lib/get-qiita-api-instance");
let currentUser;
const getCurrentUser = async () => {
    if (currentUser) {
        return currentUser;
    }
    const qiitaApi = await (0, get_qiita_api_instance_1.getQiitaApiInstance)();
    currentUser = await qiitaApi.authenticatedUser();
    return currentUser;
};
exports.getCurrentUser = getCurrentUser;
//# sourceMappingURL=get-current-user.js.map