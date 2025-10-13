"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getUrlAddress = void 0;
const getUrlAddress = (address) => {
    if (!address || typeof address === "string")
        return null;
    if (["IPv4", "IPv6"].indexOf(address.family) === -1)
        throw new Error("Unknown address family");
    return `http://${address.family === "IPv4" ? address.address : `[${address.address}]`}:${address.port}`;
};
exports.getUrlAddress = getUrlAddress;
//# sourceMappingURL=getUrlAddress.js.map