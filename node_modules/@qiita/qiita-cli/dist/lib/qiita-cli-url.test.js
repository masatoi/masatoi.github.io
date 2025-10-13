"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const qiita_cli_url_1 = require("./qiita-cli-url");
describe("apiItemsShowPath", () => {
    const itemId = "c686397e4a0f4f11683d";
    it("returns api items show path", () => {
        const url = (0, qiita_cli_url_1.apiItemsShowPath)(itemId);
        expect(url).toEqual(`/api/items/${itemId}`);
    });
    describe("when itemId is 'show' and basename param exists", () => {
        const itemId = "show";
        const basename = "newArticle";
        it("returns api items show path", () => {
            const url = (0, qiita_cli_url_1.apiItemsShowPath)(itemId, { basename: basename });
            expect(url).toEqual(`/api/items/${itemId}?basename=${basename}`);
        });
    });
});
describe("apiItemsUpdatePath", () => {
    const itemId = "c686397e4a0f4f11683d";
    it("returns api items update path", () => {
        const url = (0, qiita_cli_url_1.apiItemsUpdatePath)(itemId);
        expect(url).toEqual(`/api/items/${itemId}`);
    });
    describe("when itemId is 'show'", () => {
        const itemId = "show";
        it("returns api items update path", () => {
            const url = (0, qiita_cli_url_1.apiItemsUpdatePath)(itemId);
            expect(url).toEqual(`/api/items/post`);
        });
    });
});
describe("apiReadmeShowPath", () => {
    it("returns api readme show path", () => {
        const url = (0, qiita_cli_url_1.apiReadmeShowPath)();
        expect(url).toEqual(`/api/readme`);
    });
});
describe("itemsIndexPath", () => {
    it("returns items index path", () => {
        const url = (0, qiita_cli_url_1.itemsIndexPath)();
        expect(url).toEqual(`/`);
    });
});
describe("itemsShowPath", () => {
    const itemId = "c686397e4a0f4f11683d";
    it("returns items show path", () => {
        const url = (0, qiita_cli_url_1.itemsShowPath)(itemId);
        expect(url).toEqual(`/items/${itemId}`);
    });
    describe("when itemId is 'show' and basename param exists", () => {
        const itemId = "show";
        const basename = "newArticle";
        it("returns items show path", () => {
            const url = (0, qiita_cli_url_1.itemsShowPath)(itemId, { basename: basename });
            expect(url).toEqual(`/items/${itemId}?basename=${basename}`);
        });
    });
});
//# sourceMappingURL=qiita-cli-url.test.js.map