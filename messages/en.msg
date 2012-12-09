# Kestrel.hs
InvalidAccount: Your account is invalid.
TOC: Table of Contents

# Profile.hs
CouldntAccessAnotherUserProfile: you couldn't access another user profile.
UpdatedProfile: your profile updated.

# Wiki.hs
ConflictOccurred: Conflict occurred. Your changes couldn't be propagated.
ChangesSince time@UTCTime: Changes since #{showDate time}.
ChangesBetween from@UTCTime to@UTCTime: Changes between #{showDate from} and #{showDate to}
More: (read more...)

# default-layout
Tools: Tools
Go: Go
SearchPlaceholder: Search...
UploadFile: Upload file
Login: Login
FailToUploadFile: Fail to upload file

# default-layout.julius
RecentChanges: Recent Changes
Gallery: Gallery
Editing page@Text: #{page} Editing.
WarnFieldsRequired: Fields are required.

# wiki.julius
ShowTOC: show TOC
HideTOC: hide TOC
NullText: *** no comment log ***

# header
NowLogin: You are now logged in.
Logout: Logout
ChangePassword: Set password.
FileGallery: Upload File
ChangeProf: change your nickname.
SimpleFileUploader: simple file uploader

# login
AccountID: Account ID
Password: Password
LoginViaAccount: Login via account
LoginViaOpenID: Login via OpenID

# profile
ProfileSettings: Profile Settings
NickName: Nickname

# wiki
VIEW: View
EDIT: Edit
DELETE: Delete
HISTORY: History
TOP: Top
ALLPAGES: All Pages
RECENTCHANGES: Recent Changes
LastEditor: Editor
LastUpdatedDate: Updated Date
Preview: preview
MorePreview: more preview
Update: update
CreateNew: create new
REVERT: Revert
ChangeLog: ChangeLog
ChangeLogDescription: Please write brief notes to help you check the history of changes.
Warning: Warning!
DontRegisterRecentChanges: Don't register to `Recent Changes'.
PageLinks: Page Links
CreateNewPage: Create New Page
NewPage: New Page

# deleteWiki
ConfirmRemove path@Text: Confirm removal of `#{path}'.
RemoveAnyway: Remove anyway
WarnForDelete: Never be restored completely been removed and updated with the history of this content can not delete.
ConfirmDeleteLastChance: Do you want to delete it?

# viewNew
NotYetThePage path@Text: Page of `#{path}' yet.
IfYouWantToCreate: If you want to create,
ClickHere: Click here.

#diffHistories
AddedRows: added
RemovedRows: removed
ReturnToChangeHistory: Return to Change History.

# listHistories
ChangeHistoryOf path@Text: Revision history of `#{path}'
Version: Ver.
UpdatedDate: Updated Date
Editor: Editor
ChangeLines: lines
Operation: Operation between
CommentLog: Comment
LatestVersion: Latest Version
PreviousVersion: Previous Version
PreDiffBetween: Difference between 
PostDiffBetween:
ThisVerOfTheOriginal: This version of the original 
ForEvenOlderHistory: Click here for even older history.

# viewHistory
DiffWithTheLatestVer: Diff with the latest version.
BasedOnTheEditingOfThisEdition: Based on the editing of this edition.
BackToTheEditingHistory: Back to the editing history.

# editHistory
EditingHistory path@Text: Editing `#{path}'.
WrittenBackToTheOriginalVer v@Int: Written back to the original Ver.#{show v}.

# previewHistory
PreviewOfThe page@Text: Preview of the `#{page}.

# revertHistory
RewindToThePreviousVerOfTheConfirmationDisplayOf path@Text: Rewind to the previous version of the confirmation display of `#{path}'.
Rewind v@Int: Rewind Ver.#{show v}.
ConfirmRewindLastChance: Do you want to rewind to a previous version of this?
RewindToThisEdition: Rewind to this edition.

# systemBatch
SystemBatch: System Batch
RebuildThumbnailHeader: Rebuild Thumbnail image from all uploaded images
RebuildThumbnail: Rebuild Thumbnail.

# search
SearchResultFor key@Text: Search Result for `#{key}'

# recentChanges
SecAgo sec@Integer: #{show sec} sec ago
MinAgo min@Integer: #{show min} minute ago
HourAgo hr@Integer: #{show hr} hour ago
DayAgo day@Integer: #{show day} day ago
MonthAgo month@Integer: #{show month} month ago
YearAgo year@Integer: #{show year} year ago

# all pages
AllPages: All Pages
