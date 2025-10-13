"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const check_frontmatter_type_1 = require("./check-frontmatter-type");
describe("checkFrontmatterType", () => {
    const frontMatter = {
        title: "Title",
        tags: ["Qiita", "Ruby"],
        secret: false,
        updatedAt: "",
        id: null,
        organizationUrlName: null,
        slide: false,
    };
    it("returns no errors", () => {
        const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)(frontMatter);
        expect(errorMessages).toEqual([]);
    });
    describe("checkFrontmatterTypeTitle", () => {
        describe("when title is null", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                title: null,
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
        describe("when title is empty", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({ ...frontMatter, title: "" });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
    describe("checkFrontmatterTypeTags", () => {
        describe("when tags are empty", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({ ...frontMatter, tags: [] });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
    describe("checkFrontmatterTypeSecret", () => {
        describe("when secret is true", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                secret: true,
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
    describe("checkFrontmatterTypeUpdatedAt", () => {
        describe("when updatedAt is null", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                updatedAt: null,
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
        describe("when updatedAt is date", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                updatedAt: "2023-06-13T10:01:44+09:00",
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
    describe("checkFrontmatterTypeOrganizationUrlName", () => {
        describe("when id is empty", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({ ...frontMatter, id: "" });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
        describe("when id is uuid", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                id: "42dc00fafa166fa73d01",
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
    describe("checkFrontmatterTypeId", () => {
        describe("when organizationUrlName is empty", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                organizationUrlName: "",
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
        describe("when organizationUrlName is qiita-inc", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                organizationUrlName: "qiita-inc",
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
    describe("checkFrontmatterTypeSlide", () => {
        describe("when slide is String", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                slide: "true",
            });
            it("returns errors", () => {
                expect(errorMessages.length).toEqual(1);
            });
        });
        describe("when slide is true", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                slide: true,
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
        describe("when slide is false", () => {
            const errorMessages = (0, check_frontmatter_type_1.checkFrontmatterType)({
                ...frontMatter,
                slide: false,
            });
            it("returns no errors", () => {
                expect(errorMessages).toEqual([]);
            });
        });
    });
});
//# sourceMappingURL=check-frontmatter-type.test.js.map