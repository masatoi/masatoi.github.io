"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.checkFrontmatterType = void 0;
const checkFrontmatterType = (frontMatter) => {
    const checkFrontMatterTypes = [
        checkTitle,
        checkTags,
        checkSecret,
        checkUpdatedAt,
        checkId,
        checkOrganizationUrlName,
        checkSlide,
    ];
    return getErrorMessages(frontMatter, checkFrontMatterTypes);
};
exports.checkFrontmatterType = checkFrontmatterType;
const checkTitle = {
    getMessage: () => "titleは文字列で入力してください",
    isValid: ({ title }) => {
        return title === null || typeof title === "string";
    },
};
const checkTags = {
    getMessage: () => "tagsは配列で入力してください",
    isValid: ({ tags }) => {
        return Array.isArray(tags);
    },
};
const checkSecret = {
    getMessage: () => "privateの設定はtrue/falseで入力してください",
    isValid: ({ secret }) => {
        return typeof secret === "boolean";
    },
};
const checkUpdatedAt = {
    getMessage: () => "updated_atは文字列で入力してください",
    isValid: ({ updatedAt }) => {
        return updatedAt === null || typeof updatedAt === "string";
    },
};
const checkOrganizationUrlName = {
    getMessage: () => "organization_url_nameは文字列で入力してください",
    isValid: ({ organizationUrlName }) => {
        return (organizationUrlName === null || typeof organizationUrlName === "string");
    },
};
const checkSlide = {
    getMessage: () => "slideの設定はtrue/falseで入力してください（破壊的な変更がありました。詳しくはリリースをご確認ください https://github.com/increments/qiita-cli/releases/tag/v0.5.0）",
    isValid: ({ slide }) => {
        return typeof slide === "boolean";
    },
};
const checkId = {
    getMessage: () => "idは文字列で入力してください",
    isValid: ({ id }) => {
        return id === null || typeof id === "string";
    },
};
const getErrorMessages = (frontMatter, checkTypes) => {
    return checkTypes.reduce((errorMessages, checkType) => {
        if (!checkType.isValid(frontMatter)) {
            errorMessages.push(checkType.getMessage(frontMatter));
        }
        return errorMessages;
    }, []);
};
//# sourceMappingURL=check-frontmatter-type.js.map