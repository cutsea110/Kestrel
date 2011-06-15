# Kestrel.hs
InvalidAccount: あなたのアカウントは無効です.
TOC: 目次

# Profile.hs
CouldntAccessAnotherUserProfile: 他人のプロフィールを見ることができません.
UpdatedProfile: プロフィールを更新しました.

# Wiki.hs
ConflictOccurred: 競合が発生しました.あなたの変更は反映できませんでした.
ChangesSince time@UTCTime: #{showDate time} 以降の変更
ChangesBetween from@UTCTime to@UTCTime: #{showDate from} から #{showDate to} の変更

# default-layout
Tools: 検索ツール
Go: Go
SearchPlaceholder: Search...
UploadFile: ファイルをアップロードする
Login: ログイン

# default-layout.julius
RecentChanges: 最近の更新
Gallery: ファイルギャラリー
Editing page@Text: #{page}を編集する
WarnFieldsRequired: 必須入力項目です.

# wiki.julius
ShowTOC: 目次を表示する
HideTOC: 目次を非表示にする
NullText: *** 変更メモなし ***

# header
NowLogin: ログイン中
Logout: ログアウト
ChangePassword: パスワード変更
FileGallery: アップロードファイル
ChangeProf: プロフィール変更はこちらから
SimpleFileUploader: 簡易ファイルアップローダ

# login
AccountID: アカウントID
Password: パスワード
LoginViaAccount: このアカウントでログインする
LoginViaOpenID: OpenIDでログインする

# profile
ProfileSettings: プロフィール設定
NickName: ニックネーム

# wiki
VIEW: 表示
EDIT: 編集
DELETE: 削除
HISTORY: 更新履歴
TOP: トップ
LastEditor: 最終更新者
LastUpdatedDate: 最終更新日時
Preview: プレビューする
MorePreview: もう一度プレビューする
Update: 更新する
CreateNew: 新規作成する
REVERT: 巻き戻し
ChangeLog: 変更メモ
ChangeLogDescription: 編集履歴で確認しやすいように簡単なメモを書いてください.
Warning: 警告!

# deleteWiki
ConfirmRemove path@Text: `#{path}' の削除確認.
RemoveAnyway: それでも削除する
WarnForDelete: このコンテンツを削除すると更新履歴も含めて完全に削除されて二度と元に戻すことは出来ません.
ConfirmDeleteLastChance: 本当に削除しますか?

#diffHistories
AddedRows: 追加行
RemovedRows: 削除行
ReturnToChangeHistory: 更新履歴に戻る.

# listHistories
ChangeHistoryOf path@Text: `#{path}' の更新履歴.
Version: 版
UpdatedDate: 更新日時
Editor: 更新者
ChangeLines: 変更行数
Operation: オペレーション
CommentLog: 変更メモ
LatestVersion: 最新版
PreviousVersion: 前の版
PreDiffBetween:
PostDiffBetween: との差分
ThisVerOfTheOriginal: この版を元に
ForEvenOlderHistory: より古い履歴はこちら.

# editHistory
EditingHistory path@Text: `#{path}' を編集中.
WrittenBachToTheOriginalVer v@Int: 第 #{show v} 版を元に書き戻した.

# previewHistory
PreviewOfThe page@Text: `#{page} のプレビュー中.

# revertHistory
RewindToThePreviousVerOfTheConfirmationDisplayOf path@Text: `#{path}' の表示中の旧版への巻き戻し確認.
Rewind v@Int: 第 #{show v} 版に巻き戻し.
ConfirmRewindLastChance: この旧版に巻き戻しをしますか?
RewindToThisEdition: この版に巻き戻す.
