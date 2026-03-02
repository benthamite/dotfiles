# Security Check Patterns Reference

This reference contains all grep patterns and commands used for Mac app security auditing, organized by category.

---

## 1. Network Endpoint Discovery

### Find all URLs in source code
```bash
grep -rE "https?://[^\"'>\s]+" . --include="*.js" --include="*.json" --include="*.html" | grep -v node_modules | sort -u
```

### Find URLs in binary strings
```bash
strings -a /path/to/binary | grep -E "https?://"
```

### Suspicious URL patterns
```bash
# Discord webhooks (common for data exfiltration)
grep -rE "discord\.com/api/webhooks" .

# Telegram bots
grep -rE "api\.telegram\.org" .

# Pastebin (code/data sharing)
grep -rE "pastebin\.com" .

# Ngrok tunnels
grep -rE "ngrok\.io" .

# Raw IP addresses
grep -rE "https?://[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}" .
```

### WebSocket connections
```bash
grep -rE "wss?://[^\"'>\s]+" .
```

---

## 2. Obfuscation Detection

### Dynamic code execution
```bash
# eval usage
grep -rn "eval(" . --include="*.js" | grep -v node_modules

# Function constructor
grep -rn "new Function(" . --include="*.js" | grep -v node_modules

# With statement (often used in obfuscation)
grep -rn "with\s*(" . --include="*.js" | grep -v node_modules
```

### Encoding patterns
```bash
# Base64 decoding
grep -rn "atob(" . --include="*.js" | grep -v node_modules

# Character code conversion
grep -rn "fromCharCode" . --include="*.js" | grep -v node_modules

# Hex escape sequences
grep -rE "\\\\x[0-9a-fA-F]{2}" . --include="*.js" | grep -v node_modules

# Unicode escapes
grep -rE "\\\\u[0-9a-fA-F]{4}" . --include="*.js" | grep -v node_modules
```

### Minification indicators
```bash
# Lines over 1000 characters (likely minified)
find . -name "*.js" -not -path "*/node_modules/*" -exec awk 'length > 1000 {print FILENAME": line "NR" ("length" chars)"; exit}' {} \;

# Variable names like _0x prefixed (common obfuscators)
grep -rE "_0x[a-f0-9]{4,}" . --include="*.js" | grep -v node_modules
```

---

## 3. Hardcoded Secrets

### Cloud provider keys
```bash
# AWS Access Key ID
grep -rE "AKIA[0-9A-Z]{16}" .

# AWS Secret Key (context)
grep -rniE "aws.?secret.?access.?key" .

# Google API Key
grep -rE "AIza[0-9A-Za-z\\-_]{35}" .

# Azure connection strings
grep -rE "DefaultEndpointsProtocol=https;AccountName=" .
```

### Platform tokens
```bash
# GitHub tokens
grep -rE "gh[pousr]_[A-Za-z0-9]{36}" .

# Slack tokens
grep -rE "xox[baprs]-[0-9]+-[A-Za-z0-9]+" .

# Stripe live keys
grep -rE "sk_live_[0-9a-zA-Z]{24}" .

# Stripe test keys (less critical but still notable)
grep -rE "sk_test_[0-9a-zA-Z]{24}" .

# Twilio
grep -rE "SK[0-9a-fA-F]{32}" .
```

### Authentication secrets
```bash
# Private keys
grep -rE "BEGIN (RSA|DSA|EC|OPENSSH) PRIVATE KEY" .

# JWT tokens (base64.base64.signature)
grep -rE "eyJ[A-Za-z0-9_-]*\.eyJ[A-Za-z0-9_-]*\.[A-Za-z0-9_-]*" .

# Generic secrets in config
grep -rniE "(password|secret|api.?key|token|auth)\s*[=:]\s*['\"][^'\"]{8,}" .
```

### Database credentials
```bash
# MongoDB connection strings
grep -rE "mongodb(\+srv)?://[^\"'\s]+" .

# PostgreSQL/MySQL connection strings
grep -rE "(postgres|mysql)://[^\"'\s]+" .

# Redis URLs
grep -rE "redis://[^\"'\s]+" .
```

---

## 4. Electron Security Checks

### Critical security settings
```bash
# nodeIntegration enabled (CRITICAL)
grep -rn "nodeIntegration:\s*true" . --include="*.js"

# contextIsolation disabled (CRITICAL)
grep -rn "contextIsolation:\s*false" . --include="*.js"

# webSecurity disabled (HIGH)
grep -rn "webSecurity:\s*false" . --include="*.js"

# sandbox disabled (HIGH)
grep -rn "sandbox:\s*false" . --include="*.js"
```

### Other security concerns
```bash
# Allow insecure content (MEDIUM)
grep -rn "allowRunningInsecureContent:\s*true" . --include="*.js"

# Remote module enabled (MEDIUM, deprecated)
grep -rn "enableRemoteModule:\s*true" . --include="*.js"

# Node integration in workers (HIGH)
grep -rn "nodeIntegrationInWorker:\s*true" . --include="*.js"

# Node integration in subframes (HIGH)
grep -rn "nodeIntegrationInSubFrames:\s*true" . --include="*.js"
```

### Preload scripts
```bash
# Find preload script declarations
grep -rn "preload:" . --include="*.js" | head -20

# Check for contextBridge usage (good sign)
grep -rn "contextBridge.exposeInMainWorld" . --include="*.js"
```

### Content Security Policy
```bash
# Find CSP headers
grep -rn "Content-Security-Policy" . --include="*.js" --include="*.html"

# Check for unsafe-inline/unsafe-eval (weak CSP)
grep -rn "unsafe-inline\|unsafe-eval" . --include="*.js" --include="*.html"
```

---

## 5. Shell Execution Patterns

### Node.js
```bash
# child_process module usage
grep -rn "require.*child_process\|from.*child_process" . --include="*.js" | grep -v node_modules

# Direct execution functions
grep -rn "exec(\|execSync(\|execFile(\|spawn(\|spawnSync(" . --include="*.js" | grep -v node_modules

# Shell libraries
grep -rn "require.*shelljs\|execa" . --include="*.js" | grep -v node_modules
```

### Python (for Python-based apps)
```bash
grep -rn "subprocess\|os.system\|os.popen\|commands.getoutput" . --include="*.py"
```

### Electron shell API
```bash
# Opening external URLs/files
grep -rn "shell.openExternal\|shell.openPath" . --include="*.js" | grep -v node_modules
```

### AppleScript execution
```bash
strings -a /path/to/binary | grep -iE "(NSAppleScript|osascript)"
```

---

## 6. Privacy-Sensitive API Usage

### TCC-protected resources (in binaries)
```bash
# Screen capture
strings -a binary | grep -iE "(CGWindow|SCStream|CGDisplayStream|ScreenCapture)"

# Camera/Microphone
strings -a binary | grep -iE "(AVCaptureDevice|AVAudioSession|CMIODevice)"

# Location
strings -a binary | grep -iE "(CLLocationManager|CoreLocation)"

# Contacts
strings -a binary | grep -iE "(CNContact|ABAddressBook)"

# Calendar
strings -a binary | grep -iE "(EKEvent|EventKit)"

# Photos
strings -a binary | grep -iE "(PHPhoto|PHAsset)"

# Keychain
strings -a binary | grep -iE "(SecKeychain|SecItem|kSecClass)"

# Accessibility
strings -a binary | grep -iE "(AXUIElement|AXIsProcessTrusted)"

# Input monitoring (keylogging)
strings -a binary | grep -iE "(CGEventTap|NSEvent.addGlobalMonitor|IOHIDManager)"
```

---

## 7. Persistence Mechanisms

### LaunchAgent/Daemon indicators
```bash
# Find embedded plists
find /path/to/App.app -name "*.plist" -exec grep -l "RunAtLoad\|KeepAlive\|ProgramArguments" {} \;

# Binary strings indicating persistence
strings -a binary | grep -iE "(LaunchAgent|LaunchDaemon|LoginItem|LSSharedFileList|SMLoginItem)"
```

### Startup registration
```bash
# Login item APIs
strings -a binary | grep -iE "(LaunchAtLogin|StartAtLogin|LSSharedFileList)"

# Service Management
strings -a binary | grep -iE "(SMAppService|SMJobBless)"
```

### Helper tools
```bash
# LaunchServices directory
ls /path/to/App.app/Contents/Library/LaunchServices/ 2>/dev/null

# XPC Services
ls /path/to/App.app/Contents/XPCServices/ 2>/dev/null
```

---

## 8. Binary Analysis

### Basic info
```bash
# File type and architecture
file /path/to/binary

# Linked libraries
otool -L /path/to/binary

# Check for private frameworks (unusual)
otool -L /path/to/binary | grep -i "PrivateFrameworks"
```

### Symbol extraction
```bash
# All symbols (if not stripped)
nm -m /path/to/binary | head -100

# Undefined symbols (external dependencies)
nm -u /path/to/binary | head -50

# Look for interesting function names
nm /path/to/binary | grep -iE "(keychain|password|network|http|upload|download|encrypt|decrypt)"
```

### String extraction
```bash
# All printable strings
strings -a /path/to/binary > strings_output.txt

# Search for specific patterns
grep -E "https?://" strings_output.txt
grep -iE "(password|secret|key|token)" strings_output.txt
grep -E "/Users|/Library|~/|\.ssh|\.aws" strings_output.txt
```

---

## 9. Update Mechanism Analysis

### Electron auto-updater
```bash
# Find update configuration
grep -rn "autoUpdater\|electron-updater" . --include="*.js" | grep -v node_modules

# Find update URLs
grep -rn "update.*url\|feed.*url" . --include="*.js" --include="*.json" | grep -v node_modules
```

### Update config files
```bash
# Common update config locations
cat app-update.yml 2>/dev/null
cat dev-app-update.yml 2>/dev/null
grep -rn "publish.*provider" . --include="*.json" | grep -v node_modules
```

---

## 10. Quick Reference Commands

| Task | Command |
|------|---------|
| Extract asar | `npx asar extract app.asar ./extracted` |
| All URLs | `grep -rE "https?://" . \| sort -u` |
| Signing info | `codesign -dv /path/to/App.app` |
| Entitlements | `codesign -d --entitlements - /path/to/App.app` |
| Notarization | `spctl --assess --type execute /path/to/App.app` |
| Libraries | `otool -L /path/to/binary` |
| Strings | `strings -a /path/to/binary` |
| Find secrets | `grep -rE "AKIA\|sk_live_\|gh[pousr]_" .` |

---

## Notes

- Always exclude `node_modules` when scanning Electron apps
- Binary strings may contain false positives from linked libraries
- Some patterns may match legitimate code - context is important
- Combine automated scanning with manual review for accuracy
