"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.handleError = void 0;
const qiita_api_1 = require("../qiita-api");
const handleError = async (error) => {
    const chalk = (await import("chalk")).default;
    switch (error.name) {
        // Qiita API
        case qiita_api_1.QiitaFetchError.name:
            console.error(chalk.red.bold("Qiita APIへのリクエストでネットワークエラーが発生しました"));
            console.error(chalk.red("  インターネットに接続されているかご確認ください"));
            break;
        case qiita_api_1.QiitaBadRequestError.name:
            console.error(chalk.red.bold("Qiita APIへのリクエストに失敗しました"));
            console.error(chalk.red("  記事ファイルに不備がないかご確認ください"));
            break;
        case qiita_api_1.QiitaUnauthorizedError.name:
            console.error(chalk.red.bold("Qiitaの認証に失敗しました"));
            console.error(chalk.red("  loginコマンドでQiitaにログインしているかご確認ください"));
            console.error(chalk.red("  Qiitaのアクセストークンが正しいかご確認ください"));
            break;
        case qiita_api_1.QiitaForbiddenError.name:
            console.error(chalk.red.bold("Qiita APIへのリクエストに失敗しました"));
            console.error(chalk.red("  Qiitaのアクセストークンが正しいかご確認ください"));
            console.error(chalk.red(""));
            break;
        case qiita_api_1.QiitaForbiddenOrBadRequestError.name:
            console.error(chalk.red.bold("Qiita APIへのリクエストに失敗しました"));
            console.error(chalk.red("  記事ファイルに不備がないかご確認ください"));
            console.error(chalk.red("  または、Qiitaのアクセストークンが正しいかご確認ください"));
            console.error(chalk.red(""));
            break;
        case qiita_api_1.QiitaNotFoundError.name:
            console.error(chalk.red.bold("記事が見つかりませんでした"));
            console.error(chalk.red("  Qiita上で記事が削除されていないかご確認ください"));
            break;
        case qiita_api_1.QiitaRateLimitError.name:
            console.error(chalk.red.bold("Qiita APIのレートリミットに達しました"));
            console.error(chalk.red("  しばらく時間を置いてから再度お試しください"));
            break;
        case qiita_api_1.QiitaInternalServerError.name:
            console.error(chalk.red.bold("Qiitaのサーバーでエラーが発生しました"));
            console.error(chalk.red("  しばらく時間を置いてから再度お試しください"));
            break;
        case qiita_api_1.QiitaUnknownError.name:
            console.error(chalk.red.bold("Qiita APIへのリクエストで不明なエラーが発生しました"));
            console.error(chalk.red("  バグの可能性がある場合は、Qiita Discussionsよりご報告いただけると幸いです"));
            console.error(chalk.red("  https://github.com/increments/qiita-discussions"));
            break;
        default:
            console.error(chalk.red.bold(`エラーが発生しました (${error.message})`));
            console.error(chalk.red("  バグの可能性がある場合は、Qiita Discussionsよりご報告いただけると幸いです"));
            console.error(chalk.red("  https://github.com/increments/qiita-discussions"));
    }
};
exports.handleError = handleError;
//# sourceMappingURL=error-handler.js.map