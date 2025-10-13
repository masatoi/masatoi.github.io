"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.QiitaForbiddenOrBadRequestError = exports.QiitaUnknownError = exports.QiitaInternalServerError = exports.QiitaRateLimitError = exports.QiitaNotFoundError = exports.QiitaForbiddenError = exports.QiitaUnauthorizedError = exports.QiitaBadRequestError = exports.QiitaFetchError = void 0;
class QiitaFetchError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaFetchError";
    }
}
exports.QiitaFetchError = QiitaFetchError;
class QiitaBadRequestError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaBadRequestError";
    }
}
exports.QiitaBadRequestError = QiitaBadRequestError;
class QiitaUnauthorizedError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaUnauthorizedError";
    }
}
exports.QiitaUnauthorizedError = QiitaUnauthorizedError;
class QiitaForbiddenError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaForbiddenError";
    }
}
exports.QiitaForbiddenError = QiitaForbiddenError;
class QiitaNotFoundError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaNotFoundError";
    }
}
exports.QiitaNotFoundError = QiitaNotFoundError;
class QiitaRateLimitError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaRateLimitError";
    }
}
exports.QiitaRateLimitError = QiitaRateLimitError;
class QiitaInternalServerError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaInternalServerError";
    }
}
exports.QiitaInternalServerError = QiitaInternalServerError;
class QiitaUnknownError extends Error {
    constructor(message) {
        super(message);
        this.name = "QiitaUnknownError";
    }
}
exports.QiitaUnknownError = QiitaUnknownError;
class QiitaForbiddenOrBadRequestError extends Error {
    constructor(message, options) {
        // @ts-expect-error This error is fixed in ES2022.
        super(message, options);
        this.name = "QiitaForbiddenOrBadRequestError";
    }
}
exports.QiitaForbiddenOrBadRequestError = QiitaForbiddenOrBadRequestError;
//# sourceMappingURL=errors.js.map