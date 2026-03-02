---
name: audit-mac-app
description: This skill should be used when the user asks to audit a Mac app, check if an app is safe, or scan for malware. Focuses on apps requesting sensitive permissions.
---

# Mac App Security Audit

This skill provides a systematic approach to auditing macOS applications before running them, with particular focus on apps requesting sensitive permissions (screen recording, camera, microphone, accessibility, etc.).

## When to Use This Skill

Use this workflow when:
- Installing apps that request sensitive permissions (screen recording, accessibility)
- Apps from unknown or less-established developers
- Apps that auto-update and you want to verify each version
- Any situation where you want to understand what an app does before granting permissions

## Quick Start

Run the automated scanner:
```bash
~/.claude/skills/audit-mac-app/scripts/audit-mac-app.sh /Applications/AppName.app
```

After the scan completes, ask Claude to save a markdown report if you want to keep a copy.

Or follow the manual workflow below for deeper analysis.

## Core Workflow

### Phase 1: Basic Trust Verification

Check code signing and notarization:

```bash
APP_PATH="/Applications/AppName.app"

# Code signing details
codesign -dv --verbose=4 "$APP_PATH" 2>&1

# Notarization status
spctl --assess --verbose --type execute "$APP_PATH"

# Verify signature integrity
codesign -vvv --deep --strict "$APP_PATH"
```

**What to look for:**
- `Authority=Developer ID Application: [Company Name]` - Identifies the developer
- `Notarization Ticket=stapled` - Apple has scanned and approved
- `flags=0x10000(runtime)` - Hardened Runtime enabled (good)
- Any validation errors indicate tampering or unsigned code

### Phase 2: Permission Analysis

Extract and review entitlements:

```bash
# Get all entitlements
codesign -d --entitlements - "$APP_PATH" 2>&1

# Check Info.plist for permission descriptions
plutil -p "$APP_PATH/Contents/Info.plist" | grep -E "NS.*UsageDescription|Privacy"
```

**Risk classification** (see `references/entitlements-guide.md` for full list):

| Risk | Entitlement |
|------|-------------|
| CRITICAL | `disable-library-validation`, `allow-dyld-environment-variables` |
| HIGH | `screen-capture`, `accessibility`, `allow-unsigned-executable-memory` |
| MEDIUM | `allow-jit`, `camera`, `microphone` |
| LOW | `app-sandbox`, `files.user-selected` |

### Phase 3: App Type Detection

Identify the application framework:

```bash
# Check for Electron
ls "$APP_PATH/Contents/Frameworks/Electron Framework.framework" 2>/dev/null && echo "Electron app detected"
ls "$APP_PATH/Contents/Resources/app.asar" 2>/dev/null && echo "Electron app detected (asar)"

# Check for other frameworks
otool -L "$APP_PATH/Contents/MacOS/"* 2>/dev/null | grep -E "Qt|WebKit|Chromium" | head -5

# App type summary
file "$APP_PATH/Contents/MacOS/"*
```

| Framework | Auditability |
|-----------|--------------|
| **Electron** | High - JavaScript source extractable |
| **Tauri** | Medium - Rust binary + web UI |
| **Native Swift/ObjC** | Low - Compiled binary |
| **Qt** | Low - Compiled C++ |
| **Java** | Medium - JAR files decompilable |

### Phase 4: Code Extraction (Electron Apps)

For Electron apps, extract the source code:

```bash
APP_NAME=$(basename "$APP_PATH" .app)
AUDIT_DIR="$HOME/.claude/app-audits/$APP_NAME/$(date +%Y%m%d)"
mkdir -p "$AUDIT_DIR"

# Extract asar
npx asar extract "$APP_PATH/Contents/Resources/app.asar" "$AUDIT_DIR/extracted"

# Also check unpacked resources
cp -r "$APP_PATH/Contents/Resources/app.asar.unpacked" "$AUDIT_DIR/" 2>/dev/null
```

### Phase 5: Automated Security Scan

Run the automated scanner:

```bash
~/.claude/skills/audit-mac-app/scripts/audit-mac-app.sh "$APP_PATH"
```

Or run individual checks manually:

**Network endpoints:**
```bash
grep -rE "https?://" "$AUDIT_DIR/extracted" --include="*.js" 2>/dev/null | grep -v node_modules | sort -u
```

**Obfuscation patterns:**
```bash
grep -rE "(eval\(|new Function\(|atob\(|\\\\x[0-9a-f]{2})" "$AUDIT_DIR/extracted" --include="*.js" 2>/dev/null | grep -v node_modules
```

**Hardcoded secrets:**
```bash
# AWS keys
grep -rE "AKIA[0-9A-Z]{16}" "$AUDIT_DIR/extracted" 2>/dev/null

# API keys/tokens
grep -rniE "(api[_-]?key|secret|token)\s*[=:]\s*['\"][a-zA-Z0-9]{20,}" "$AUDIT_DIR/extracted" 2>/dev/null
```

### Phase 6: Electron-Specific Security Checks

For Electron apps, check for dangerous configurations:

```bash
cd "$AUDIT_DIR/extracted"

# CRITICAL: Check for disabled security
grep -rn "nodeIntegration:\s*true" . --include="*.js"
grep -rn "contextIsolation:\s*false" . --include="*.js"
grep -rn "webSecurity:\s*false" . --include="*.js"
grep -rn "sandbox:\s*false" . --include="*.js"

# Check preload scripts
grep -rn "preload:" . --include="*.js" | head -10

# Content Security Policy
grep -rn "Content-Security-Policy" . --include="*.js" --include="*.html"
```

**Severity Matrix:**

| Setting | Secure | Insecure | Severity |
|---------|--------|----------|----------|
| `nodeIntegration` | `false` | `true` | **CRITICAL** |
| `contextIsolation` | `true` | `false` | **CRITICAL** |
| `sandbox` | `true` | `false` | HIGH |
| `webSecurity` | `true` | `false` | HIGH |

### Phase 7: Binary Analysis (All Apps)

For native apps or to supplement Electron audits:

```bash
BINARY="$APP_PATH/Contents/MacOS/"*

# Extract strings (URLs, paths, keywords)
strings -a $BINARY | grep -E "https?://" | sort -u
strings -a $BINARY | grep -iE "(password|secret|key|token|auth)" | head -20

# Linked libraries
otool -L $BINARY

# Check for private frameworks (potential red flag)
otool -L $BINARY | grep -i "PrivateFrameworks"

# Persistence indicators
strings -a $BINARY | grep -iE "(LaunchAgent|LoginItem|LSSharedFileList)"
```

### Phase 8: Report Generation

Save the audit results:

```bash
cat > "$AUDIT_DIR/audit-report.md" << 'EOF'
# Security Audit Report

## App Details
- **Name**: [APP_NAME]
- **Version**: [VERSION]
- **Developer**: [DEVELOPER]
- **Audit Date**: [DATE]

## Trust Status
- Code Signed: [Yes/No]
- Notarized: [Yes/No]
- Hardened Runtime: [Yes/No]

## Entitlements
[List entitlements with risk levels]

## Findings

### CRITICAL
[List findings]

### HIGH
[List findings]

### MEDIUM
[List findings]

## Network Endpoints
[List all URLs discovered]

## Recommendations
[Actionable items]

## Limitations
[What could not be verified]
EOF
```

## Version Comparison

To compare between app versions after an update:

```bash
# Extract both versions to temp directories
npx asar extract /Applications/OldApp.app/Contents/Resources/app.asar /tmp/old-version
npx asar extract /Applications/NewApp.app/Contents/Resources/app.asar /tmp/new-version

# Diff the extracted source
diff -r /tmp/old-version /tmp/new-version | head -100

# Check for new network endpoints
grep -rh "https://" /tmp/new-version --include="*.js" | sort -u > /tmp/new-urls.txt
grep -rh "https://" /tmp/old-version --include="*.js" | sort -u > /tmp/old-urls.txt
comm -13 /tmp/old-urls.txt /tmp/new-urls.txt
```

## Quick Reference

| Check | Command |
|-------|---------|
| Signing | `codesign -dv "$APP_PATH"` |
| Notarization | `spctl --assess --type execute "$APP_PATH"` |
| Entitlements | `codesign -d --entitlements - "$APP_PATH"` |
| App type | `file "$APP_PATH/Contents/MacOS/"*` |
| Extract Electron | `npx asar extract app.asar ./extracted` |
| Find URLs | `grep -rE "https?://" . \| sort -u` |
| Run full audit | `~/.claude/skills/audit-mac-app/scripts/audit-mac-app.sh "$APP_PATH"` |

## Limitations

This audit **CANNOT** detect:

| Limitation | Explanation |
|------------|-------------|
| Obfuscated native code | Compiled binaries hide logic |
| Encrypted payloads | Malware can decrypt at runtime |
| Server-side changes | App may fetch malicious code later |
| Time-delayed behavior | Some malware activates after a delay |
| Post-permission behavior | Can't verify what happens after access granted |

**When to recommend professional audit:**
- Enterprise deployment affecting many users
- Apps handling sensitive/regulated data
- Apps with CRITICAL findings you still need to use
- Suspected compromise or malware

## Additional Resources

- `references/entitlements-guide.md` - Full entitlement risk classification
- `references/check-patterns.md` - All grep patterns with explanations
