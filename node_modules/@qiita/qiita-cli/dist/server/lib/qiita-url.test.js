"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const qiita_url_1 = require("./qiita-url");
describe("itemUrl", () => {
    const id = "c686397e4a0f4f11683d";
    const userId = "Qiita";
    it("returns item url", () => {
        const url = (0, qiita_url_1.itemUrl)({ id, userId });
        expect(url).toEqual(`https://qiita.com/${userId}/items/${id}`);
    });
});
//# sourceMappingURL=qiita-url.test.js.map