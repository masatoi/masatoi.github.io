---
layout: post
title: "Codex CLIでClojure開発する際のメモ: cljコマンドがsandbox下で動かないとき"
description: ""
category: 
tags: [lisp,clojure]
---
{% include JB/setup %}

# Codex CLI の非対話シェル環境で `clj` コマンドを動かす設定方法

Codex CLI や CI/CD 環境などの「非対話シェル」では、`~/.bashrc` 内の `brew shellenv` が `ps` コマンドを実行するため、サンドボックス制限でエラーを起こすことがあります。
これを回避しつつ Homebrew 経由でインストールしたコマンドを利用するには、**非対話シェル時のみ固定パスを設定して即終了する**のが最も安全です。

以下は、実際に Codex CLI 環境で `clj` コマンドを正常に動かせた設定例です。

---

## 設定例：`~/.bashrc`

```bash
# For non-interactive shells (e.g. Codex CLI): 
# pass only the fixed PATH of Homebrew and exit immediately
case $- in
  *i*) ;;
  *)
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/bin:/bin${PATH:+:$PATH}"
    export PATH="$HOME/local/bin:$HOME/bin:$HOME/.local/bin:/usr/local/bin/:/opt/homebrew/opt/openjdk/bin:$PATH"
    return
    ;;
esac
# From this point down, it is executed only in an interactive shell.

# Homebrew setting
eval "$(/opt/homebrew/bin/brew shellenv)"
```

## Codex CLI サンドボックス内での clj-kondo の実行

CLI がサンドボックス内で実行される場合、lint コマンドは警告のみを出力する場合でも「失敗」することがよくあります。サンドボックスはゼロ以外の終了コードをエラーとして扱うため、clj-kondo のデフォルトの終了コード（警告の場合は 2、設定の問題の場合は 3）により、自動化処理の継続が妨げられることがあります。

### 対処方法

1. **clj-kondo の生のエイリアスを維持する。** `deps.edn` に `:lint-run` エイリアスの例を作成し、`clj-kondo.main` を指定して、希望するオプション（キャッシュディレクトリ、パスなど）を設定します。

2. **ラッパースクリプトを追加する**（例：`script/lint.clj`）。このスクリプトは：
   - `clojure -M:lint-run`（または生のエイリアス）をシェル実行し、
   - stdout/stderr をそのまま出力し、
   - 終了コードを無視してラッパーが常に `0` で終了するようにします（オプションで抑制されたコードをログに記録）。
   これにより、検出結果を保持しながらサンドボックスを正常に動作させます。

3. **公開エイリアスをラッパーに向ける。** プロジェクトの `:lint` エイリアス（または開発者が呼び出すコマンド）を変更し、`:main-opts ["-i" "script/lint.clj"]` または `-m clojure.main script/lint.clj` を使用してラッパーを読み込むようにします。

4. **lint を再実行する。** `clj -M:lint` は、Codex CLI サンドボックスの制限下でも、すべての警告/エラーを表示し、正常に終了するようになります。

同じパターンは、基礎となるコマンドが致命的でない理由でゼロ以外の終了コードを返す可能性がある場合、他のツール（`fmt`、`spec`、カスタムタスク）でも機能します。ツールをラップし、その出力を転送し、終了コードを正規化します。

lint.clj
```clojure
(require '[clojure.java.shell :as shell])

(let [args (vec *command-line-args*)
      {:keys [exit out err]} (apply shell/sh "clojure" "-M:lint-run" args)]
  (when (seq out)
    (print out)
    (flush))
  (when (seq err)
    (binding [*out* *err*]
      (print err)
      (flush)))
  (when (pos? exit)
    (binding [*out* *err*]
      (println (format "clj-kondo exited with %d; treating as success per repo policy." exit))
      (flush))))
```

deps.edn
```clojure
  :lint
  {:main-opts ["-i" "script/lint.clj"]}
  :lint-run
  {:extra-deps {clj-kondo/clj-kondo {:mvn/version "2025.09.22"}}
   :mvn/repos {"clojars" {:url "https://repo.clojars.org/"}}
   :main-opts ["-m" "clj-kondo.main" "--cache-dir" ".clj-kondo/.cache" "--lint" "src" "test"]}
```
