local apps = {}

apps.emacs = "org.gnu.Emacs"
apps.firefox = "org.mozilla.firefox"
apps.safari = "com.apple.Safari"
apps.teams = "com.microsoft.teams2"
apps.slack = "com.tinyspeck.slackmacgap"
apps.chrome = "com.google.Chrome"
apps.obsidian = "md.obsidian"
apps.cursor = "com.todesktop.230313mzl4w4u92"
apps.vscodium = "com.vscodium" -- used for sourcegraph cody
apps.vscode = "com.microsoft.VSCode"
apps.mail = "com.apple.mail"
apps.zed = "dev.zed.Zed"
apps.cal = "com.apple.iCal"

-- role assignments
apps.browser = apps.firefox
apps.notesApp = apps.obsidian
apps.editor = apps.emacs

return apps
