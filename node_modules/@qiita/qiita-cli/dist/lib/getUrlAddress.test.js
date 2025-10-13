"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const getUrlAddress_1 = require("./getUrlAddress");
describe("getUrlAddress", () => {
    describe("when null is passed", () => {
        it("returns null", () => {
            const url = (0, getUrlAddress_1.getUrlAddress)(null);
            expect(url).toBeNull();
        });
    });
    describe("when string is passed", () => {
        it("returns null", () => {
            const url = (0, getUrlAddress_1.getUrlAddress)("foobar");
            expect(url).toBeNull();
        });
    });
    describe("when IPv4 is passed", () => {
        it("returns correct url", () => {
            const address = {
                address: "0.0.0.0",
                family: "IPv4",
                port: 8888,
            };
            const url = (0, getUrlAddress_1.getUrlAddress)(address);
            expect(url).toEqual(`http://${address.address}:${address.port}`);
        });
    });
    describe("when IPv6 is passed", () => {
        it("returns correct url", () => {
            const address = {
                address: "::",
                family: "IPv6",
                port: 8888,
            };
            const url = (0, getUrlAddress_1.getUrlAddress)(address);
            expect(url).toEqual(`http://[${address.address}]:${address.port}`);
        });
    });
});
//# sourceMappingURL=getUrlAddress.test.js.map