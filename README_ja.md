# 概要
sage-shell-modeは[Sage](http://www.sagemath.org/)を
[GNU Emacs](http://www.gnu.org/software/emacs/)で使うための拡張機能です．
ほとんどがEmacs Lisp (elisp)で， ごく一部がpythonで書かれています．似た
ような機能をもつものとして，
[sage-mode](http://wiki.sagemath.org/sage-mode)があります．それにくらべ
ると，機能は少ないですが，関数名の補完が速かったり，Sageのバッファでコー
ドが色付けされたり，補完するのにanythingやauto-completeが使えたりし
ます．

# 動作環境
Emacsのバージョンは，23以降を想定しています．
Sageのバージョンは，5.5から5.8までを想定しています．

# ライセンス
ライセンスは[GPLv3](http://www.gnu.org/licenses/gpl.html)です.

# インストール
elispのscriptによる自動インストールと手動によるインストールが可能です．

## スクリプトによるインストール
次のコードを\*scratch\*バッファに貼り付けて，評価して下さい．
\*scratch\*バッファでは，一番最後の)より後にカーソルを置いて，`C-j`を
押すことによりelispの式を評価できます．
デフォルトでは，"~/.emacs.d/sage-shell"にインストールされますが，イン
ストールされる場所を変えたい場合は，下のコードの3行目を変更して下さい．
アンインストールするときは，"~/.emacs.d/sage-shell"ごと削除して下さい．

```lisp
(progn
  (setq sage-install-url "https://raw.github.com/stakemori/sage-shell-mode/master/"
        sage-install-installation-directory "~/.emacs.d/sage-shell")
  (url-retrieve
   (concat sage-install-url "sage-install.el")
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))
```

## 手動によるインストール
1. 適当なディレクトリにおいて，gitレポジトリーをクローンして下さい．また
   は，全てのファイルをダウンロードして下さい．

    ```sh
    git clone git://github.com/stakemori/sage-shell-mode.git
    cd sage-shell-mode
    ```

1. "~/.emacs.d/sage-shell"というディレクトリを作り，全てのelispファイ
   ルとpythonファイルをコピーします．

    ```sh
    mkdir ~/.emacs.d/sage-shell
    cp *.el *.py ~/.emacs.d/sage-shell
    ```
1. 以下のコードをEmacsの設定ファイルに追加して下さい．以下のコードを評
   価するか，Emacsを再起動して下さい．EmacsがSageの実行ファイルを見つけ
   られない場合，変数`sage-shell:sage-root`を明示する必要があります．そ
   の場合，変数`sage-shell:sage-root`には，SageのRoot Directoryを代入し
   て下さい．

    ```lisp
    (add-to-list 'load-path "~/.emacs.d/sage-shell")
    (require 'sage-shell-autoloads)
    (add-to-list 'auto-mode-alist (cons "\\.sage$" 'sage-mode))
    ;; You do not need the following line if Sage executable is in your PATH.
    (setq sage-shell:sage-root "/path/to/SAGE_ROOT/")
    ```

1. 新しく追加した，elispのファイルをバイトコンパイルして下さい．

1. (Optional) Sageのバッファでauto-completeを使いたい場合，以下のコー
ドをEmacsの設定ファイルに追加して下さい．これには，
`auto-complete.el`が必要です．

    ```lisp
    (setq ac-modes (append '(sage-mode sage-shell-mode) ac-modes))
    (add-hook 'sage-shell-mode-hook 'sage-shell-ac:add-sources)
    (add-hook 'sage-mode-hook 'sage-edit-ac:add-sources)
    ```
1. (Optional) Tabによる補完を，anythingで置きかえたい場合，次のコード
   をEmacsの設定ファイルに追加して下さい．これには，`anything.el` と
   `anything-match-plugin.el`が必要です．

    ```lisp
    (setq sage-shell:completion-function
          'anything-sage-shell
          sage-shell:help-completion-function
          'anything-sage-shell-describe-object-at-point)
    ```

# SageをEmacsの中で使う
## 基本操作
### Sageの起動
sage-shell-modeが正しくインストールされていれば，`M-x run-sage`でSageが起動するはずです．
また， ` M-x run-new-sage`によってSageのプロセスをいくつもたちあげることができます．

### Sageの終了
ターミナルの中と同じで， 何もかいてない行で`C-d`と入力するとSageが終了します．

### 計算の中断
`C-c C-c`によって計算を中断できます．

### コマンドの履歴
コマンドの履歴をさかのぼるには， `M-p`または`C-<up>`とします．
コマンドの履歴を進むには， `M-n`または`C-<down>`とします．
過去のコマンドの履歴を検索するには，`M-r`とします．

### タブ
タブは， 補完をしたり， インデントの深さを変えたりします．
Sageの中で複数行を入力しているとき，自動的にインデントしますが，
カーソル位置がインデントの行頭にあるとき，TABによってインデントの深さが変えられます．

### アウトプットの消去
`C-c C-o`によって直前のアウトプットを消すことができます．
また， `C-c M-o`とすることで， バッファの内容を消すことができます．

## 補完
タブによって， 現在のカーソル位置において適切な補完候補が表示されます．
変数`sage-shell:completion-function`の値が`anything-sage-shell`の場合，
anythingによる補完が行なわれます．
auto-completeをSageのバッファで使うには，以下のコードをEmacsの設定ファ
イルに置きます．

```lisp
(setq ac-modes (append '(sage-mode sage-shell-mode) ac-modes))
(add-hook 'sage-shell-mode-hook 'sage-shell-ac:add-sources)
(add-hook 'sage-mode-hook 'sage-edit-ac:add-sources)
```

`anything.el`と`anything-match-plugin.el`が`load-path`に存在する場合，
コマンド`anything-sage-shell`がつかえます．これは，anythingによって，
Sageの関数などを補完します．

## ヘルプの参照
ターミナルの中でSageを使ったときのように， 関数名などの後に?をつけることによって，
ヘルプを参照することができます．
??をつけることによって， ソースファイルをview-modeで開くことができます．
`C-c C-h`で， ヘルプの参照ができます．変数
`sage-shell:help-completion-function`の値が
`anything-sage-shell-describe-object-at-point`のとき，anythingによって
ヘルプを参照することができます．
ヘルプは\*Sage help\*というバッファに表示されます．
\*Sage help\*バッファでは， `sage-help-mode`がメジャーモードになっていて，
`help-mode`や`view-mode`のキーバインドを受けついでいます．
`sage-help-mode`では， `C-c C-j`によって「sage:」を含んでいる行をSage
のプロセスに送ることができます．

# SageのファイルをEmacsで編集，実行する
## sage-mode
Sageのファイルを編集するために，`sage-mode`というメジャーモードを作りました．
でもこれは，`python-mode`とほとんど同じで，
いくつかのキーバインドを奪っているだけです．

# ファイルをsage-modeで開く
拡張子が， sageのファイルは自動的に`sage-mode`で開かれます．
拡張子が， pyなどのファイルを`sage-mode`で開くときは， ファイルの1行目に
```
# -*- mode: sage -*-
```
と書いて下さい．これは， pythonに文字コードをつたえるやり方と併用できます．
例えば次のように書くことができます．
```
# -*- mode: sage; coding: utf-8 -*-
```

## バッファやリージョン， ファイルをSageのプロセスに送る
`sage-mode`では， バッファやリージョン， ファイルをSageのプロセスに送ることができます．
キーバインドは他の似たようなメジャーモードと同じです．
例えば，`C-c C-c`によって，バッファをSageのプロセスに送ります．
`C-c C-l`でファイルをロードします．
（ファイル名を尋ねられますが， 何も入力しないでRETを押すことにより開いているファイルをロードすることができます．）
`C-c C-r`でリージョンをSageのプロセスに送り，
`C-M-x`で現在のカーソル位置が関数を定義しているブロック内ならその関数の定義を
Sageのプロセスに送ります．また， `C-c C-z`でSageのプロセスのバッファに移ります．
以下が， `sage-mode`で定義されている主なコマンドです． 他のキーバインドは，`python-mode`と同じです．

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">
<caption></caption>
<colgroup><col class="left" /><col class="left" /><col class="left" />
</colgroup>
<tbody>
<tr><td class="left">コマンド名</td><td class="left">キー</td><td class="left">説明</td></tr>
<tr><td class="left">sage-edit:send-buffer</td><td class="left">C-c C-c</td><td class="left">バッファをSageのプロセスに送ります</td></tr>
<tr><td class="left">sage-edit:send-defun</td><td class="left">C-M-x</td><td class="left">関数の定義をSageのプロセスに送ります</td></tr>
<tr><td class="left">sage-edit:send-region</td><td class="left">C-c C-r</td><td class="left">リージョンをSageのプロセスに送ります</td></tr>
<tr><td class="left">sage-edit:send-line</td><td class="left">C-c C-j</td><td class="left">カーソルがある行をSageのプロセスに送ります</td></tr>
<tr><td class="left">sage-edit:load-file</td><td class="left">C-c C-l</td><td class="left">ファイルをロードします</td></tr>
<tr><td class="left">sage-edit:load-current-file</td><td class="left">なし</td><td class="left">開いているファイルをロードします</td></tr>
<tr><td class="left">sage-edit:attach-file</td><td class="left">なし</td><td class="left">attachによってファイルをロードします</td></tr>
<tr><td class="left">sage-edit:pop-to-process-buffer</td><td class="left">C-c C-z</td><td class="left">Sageのプロセスのバッファに移ります</td></tr>
</tbody>
</table>

この他に， `sage-edit:send-buffer-and-go`, `sage-edit:send-region-and-go`などの"-and-go"を付けた
コマンドがあります．
これらのコマンドは，バッファとかリージョンをプロセスに送ってから， プロセスのバッファに移ります．
