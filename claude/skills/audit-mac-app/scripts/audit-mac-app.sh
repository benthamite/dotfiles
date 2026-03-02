#!/bin/bash

# Mac App Security Audit Script
# Automated security scanning for macOS applications
# Usage: audit-mac-app.sh /path/to/App.app

# Don't exit on grep failures (no matches returns 1)
set +e

# Colors for terminal output
RED='\033[0;31m'
YELLOW='\033[1;33m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color
BOLD='\033[1m'

# Severity counters
CRITICAL_COUNT=0
HIGH_COUNT=0
MEDIUM_COUNT=0
LOW_COUNT=0
INFO_COUNT=0

# Output functions
critical() {
    echo -e "${RED}${BOLD}[CRITICAL]${NC} $1"
    ((CRITICAL_COUNT++))
}

high() {
    echo -e "${RED}[HIGH]${NC} $1"
    ((HIGH_COUNT++))
}

medium() {
    echo -e "${YELLOW}[MEDIUM]${NC} $1"
    ((MEDIUM_COUNT++))
}

low() {
    echo -e "${CYAN}[LOW]${NC} $1"
    ((LOW_COUNT++))
}

info() {
    echo -e "${BLUE}[INFO]${NC} $1"
    ((INFO_COUNT++))
}

success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

header() {
    echo -e "\n${BOLD}=== $1 ===${NC}"
}

subheader() {
    echo -e "\n${BOLD}$1${NC}"
}

# Check arguments
if [ -z "$1" ]; then
    echo "Usage: $0 /path/to/App.app"
    exit 1
fi

APP_PATH="$1"

if [ ! -d "$APP_PATH" ]; then
    echo "Error: $APP_PATH is not a valid directory"
    exit 1
fi

APP_NAME=$(basename "$APP_PATH" .app)
BINARY_PATH="$APP_PATH/Contents/MacOS"

# Get version from Info.plist if available
APP_VERSION=$(defaults read "$APP_PATH/Contents/Info.plist" CFBundleShortVersionString 2>/dev/null || echo "unknown")

echo -e "${BOLD}Mac App Security Audit${NC}"
echo "App: $APP_NAME"
echo "Version: $APP_VERSION"
echo "Path: $APP_PATH"
echo "Date: $(date)"
echo ""

# =============================================================================
header "Phase 1: Trust Verification"
# =============================================================================

# Code signing check
subheader "Code Signing"
CODESIGN_OUTPUT=$(codesign -dv --verbose=4 "$APP_PATH" 2>&1)
if echo "$CODESIGN_OUTPUT" | grep -q "Authority="; then
    DEVELOPER=$(echo "$CODESIGN_OUTPUT" | grep "Authority=Developer ID" | head -1 | sed 's/Authority=//')
    if [ -n "$DEVELOPER" ]; then
        success "Signed by: $DEVELOPER"
    else
        DEVELOPER=$(echo "$CODESIGN_OUTPUT" | grep "Authority=" | head -1 | sed 's/Authority=//')
        info "Signed by: $DEVELOPER"
    fi
elif echo "$CODESIGN_OUTPUT" | grep -q "Signature=adhoc"; then
    medium "Ad-hoc signed (not Developer ID)"
elif echo "$CODESIGN_OUTPUT" | grep -q "TeamIdentifier="; then
    TEAM=$(echo "$CODESIGN_OUTPUT" | grep "TeamIdentifier=" | sed 's/TeamIdentifier=//')
    success "Signed with Team ID: $TEAM"
else
    critical "App is NOT code signed"
fi

# Check for hardened runtime
if codesign -dv "$APP_PATH" 2>&1 | grep -q "flags=0x10000(runtime)"; then
    success "Hardened Runtime enabled"
else
    high "Hardened Runtime NOT enabled"
fi

# Notarization check
subheader "Notarization"
if spctl --assess --verbose --type execute "$APP_PATH" 2>&1 | grep -q "accepted"; then
    success "App is notarized and accepted by Gatekeeper"
else
    SPCTL_RESULT=$(spctl --assess --verbose --type execute "$APP_PATH" 2>&1)
    if echo "$SPCTL_RESULT" | grep -q "rejected"; then
        critical "App rejected by Gatekeeper"
    else
        medium "Notarization status unclear"
    fi
fi

# Signature integrity
subheader "Signature Integrity"
if codesign -vvv --deep --strict "$APP_PATH" 2>&1 | grep -q "valid on disk"; then
    success "Signature valid and intact"
else
    critical "Signature verification failed"
fi

# =============================================================================
header "Phase 2: Entitlements Analysis"
# =============================================================================

ENTITLEMENTS=$(codesign -d --entitlements - "$APP_PATH" 2>&1)

# Critical entitlements
if echo "$ENTITLEMENTS" | grep -q "disable-library-validation"; then
    critical "Entitlement: disable-library-validation (can load unsigned code)"
fi

if echo "$ENTITLEMENTS" | grep -q "allow-dyld-environment-variables"; then
    critical "Entitlement: allow-dyld-environment-variables (code injection risk)"
fi

if echo "$ENTITLEMENTS" | grep -q "disable-executable-page-protection"; then
    critical "Entitlement: disable-executable-page-protection"
fi

# High risk entitlements
if echo "$ENTITLEMENTS" | grep -q "screen-capture\|ScreenCapture"; then
    high "Entitlement: screen-capture (can record your screen)"
fi

if echo "$ENTITLEMENTS" | grep -q "accessibility"; then
    high "Entitlement: accessibility (can control other apps)"
fi

if echo "$ENTITLEMENTS" | grep -q "allow-unsigned-executable-memory"; then
    high "Entitlement: allow-unsigned-executable-memory"
fi

# Medium risk entitlements
if echo "$ENTITLEMENTS" | grep -q "allow-jit"; then
    medium "Entitlement: allow-jit (Just-In-Time compilation)"
fi

if echo "$ENTITLEMENTS" | grep -q "camera"; then
    medium "Entitlement: camera access"
fi

if echo "$ENTITLEMENTS" | grep -q "microphone"; then
    medium "Entitlement: microphone access"
fi

# Informational
if echo "$ENTITLEMENTS" | grep -q "app-sandbox"; then
    success "App uses sandbox (good security practice)"
else
    info "App does NOT use sandbox"
fi

# =============================================================================
header "Phase 3: App Type Detection"
# =============================================================================

IS_ELECTRON=false
EXTRACTED_PATH=""
APP_TYPE="Native"

# Check for Electron
if [ -d "$APP_PATH/Contents/Frameworks/Electron Framework.framework" ] || [ -f "$APP_PATH/Contents/Resources/app.asar" ]; then
    IS_ELECTRON=true
    APP_TYPE="Electron"
    info "App Type: Electron (JavaScript source auditable)"

    # Get Electron version
    ELECTRON_VERSION=$(cat "$APP_PATH/Contents/Frameworks/Electron Framework.framework/Versions/Current/Resources/version" 2>/dev/null || echo "unknown")
    info "Electron Version: $ELECTRON_VERSION"
elif otool -L "$BINARY_PATH"/* 2>/dev/null | grep -q "QtCore"; then
    APP_TYPE="Qt"
    info "App Type: Qt (compiled C++, limited auditability)"
elif otool -L "$BINARY_PATH"/* 2>/dev/null | grep -q "WebKit"; then
    APP_TYPE="Native+WebKit"
    info "App Type: Native with WebKit (may be Tauri or similar)"
else
    info "App Type: Native (compiled binary, limited auditability)"
fi

# =============================================================================
header "Phase 4: Source Code Extraction"
# =============================================================================

if [ "$IS_ELECTRON" = true ]; then
    AUDIT_DIR="/tmp/audit-$$"
    mkdir -p "$AUDIT_DIR"

    if [ -f "$APP_PATH/Contents/Resources/app.asar" ]; then
        echo "Extracting Electron app.asar..."
        npx --yes asar extract "$APP_PATH/Contents/Resources/app.asar" "$AUDIT_DIR/extracted" 2>/dev/null
        EXTRACTED_PATH="$AUDIT_DIR/extracted"
        success "Source code extracted for analysis"
    fi
fi

# =============================================================================
header "Phase 5: Security Pattern Scan"
# =============================================================================

if [ -n "$EXTRACTED_PATH" ] && [ -d "$EXTRACTED_PATH" ]; then
    subheader "Network Endpoints"

    # Function to infer purpose from URL
    infer_purpose() {
        local url="$1"
        local purpose="Unknown"

        # Check for suspicious URLs first
        if echo "$url" | grep -qiE "pastebin|discord\.com/api/webhooks|telegram\.org/bot"; then
            echo "Data exfiltration (suspicious)"
            return
        elif echo "$url" | grep -qiE "ngrok\.io|localhost|127\.0\.0\.1"; then
            echo "Local/tunnel endpoint"
            return
        fi

        # Infer from domain - order matters (more specific patterns first!)
        if echo "$url" | grep -qiE "cloudfunctions\.net"; then
            purpose="Cloud Functions (backend)"
        elif echo "$url" | grep -qiE "generativelanguage\.googleapis|generativeai\.googleapis"; then
            purpose="AI API (Gemini)"
        elif echo "$url" | grep -qiE "firebaseapp\.com|firebaseio\.com|firebase\.google"; then
            purpose="Firebase"
        elif echo "$url" | grep -qiE "console\.firebase"; then
            purpose="Firebase Console"
        elif echo "$url" | grep -qiE "google-analytics\.com|analytics\.google"; then
            purpose="Analytics (Google)"
        elif echo "$url" | grep -qiE "accounts\.google"; then
            purpose="Authentication (Google)"
        elif echo "$url" | grep -qiE "googleapis\.com"; then
            purpose="Google API"
        elif echo "$url" | grep -qiE "stripe\.com"; then
            purpose="Payments (Stripe)"
        elif echo "$url" | grep -qiE "segment\.com|mixpanel|amplitude"; then
            purpose="Analytics"
        elif echo "$url" | grep -qiE "sentry\.io|bugsnag|rollbar"; then
            purpose="Error tracking"
        elif echo "$url" | grep -qiE "auth0|okta|cognito"; then
            purpose="Authentication"
        elif echo "$url" | grep -qiE "openai\.com"; then
            purpose="AI API (OpenAI)"
        elif echo "$url" | grep -qiE "anthropic\.com"; then
            purpose="AI API (Anthropic)"
        elif echo "$url" | grep -qiE "langchain|smith\.langchain"; then
            purpose="LangChain"
        elif echo "$url" | grep -qiE "openrouter\.ai"; then
            purpose="AI API (OpenRouter)"
        elif echo "$url" | grep -qiE "github\.com.*releases|github\.com/[^/]+/[^/]+$"; then
            purpose="GitHub repo"
        elif echo "$url" | grep -qiE "cdn\.|jsdelivr|cloudfront|cloudflare|unpkg"; then
            purpose="CDN"
        elif echo "$url" | grep -qiE "update|upgrade|release|download"; then
            purpose="Auto-update"
        elif echo "$url" | grep -qiE "login|signin|oauth|token|auth"; then
            purpose="Authentication"
        elif echo "$url" | grep -qiE "checkout\."; then
            purpose="Payments"
        elif echo "$url" | grep -qiE "api\.|/api/|/v[0-9]+"; then
            purpose="API endpoint"
        elif echo "$url" | grep -qiE "\.js$|\.css$|\.woff|\.png|\.jpg"; then
            purpose="Static assets"
        fi

        # Try to identify app's own domain (using app name)
        local app_name_lower=$(echo "$APP_NAME" | tr '[:upper:]' '[:lower:]')
        if echo "$url" | grep -qiE "$app_name_lower\.(ai|com|io|app|co)"; then
            if [ "$purpose" = "Unknown" ]; then
                purpose="App website/backend"
            fi
        fi

        echo "$purpose"
    }

    # Get unique URLs - extract from quoted strings, filter noise
    URLS=$(grep -rohE '"https?://[^"]+"|'"'"'https?://[^'"'"']+'"'" "$EXTRACTED_PATH" --include="*.js" --include="*.json" 2>/dev/null \
        | grep -v node_modules \
        | sed 's/^["'"'"']//; s/["'"'"']$//' \
        | grep -vE '\$\{|placeholder|example\.com|foo\.com|foo\.bar' \
        | grep -vE 'localhost|127\.0\.0\.1|\[ffff' \
        | grep -vE 'stackoverflow|wikipedia|mozilla\.org|w3\.org|whatwg\.org' \
        | grep -vE 'jsperf|codepen|jsfiddle|chromium\.org|bugs\.' \
        | grep -vE 'github\.com.*(issues|pull|blob|tree|compare)' \
        | grep -vE 'json-schema\.org|schema\.org|purl\.org' \
        | grep -vE '\.md$|\.txt$|/docs/|/documentation' \
        | grep -vE 'juliangruber|philipwalton|substack\.net|n8\.io|ajv\.js' \
        | grep -vE 'netlify\.app|cocodataset|images\.' \
        | grep -vE '^https?://foo$|^http://foo$|feross\.org' \
        | grep -vE 'gstatic\.com|http://github\.com' \
        | grep -vE '^https?://\.\.\.' \
        | sort -u \
        | head -25)

    if [ -n "$URLS" ]; then
        # Terminal output header
        echo ""
        printf "%-60s %s\n" "ENDPOINT" "PURPOSE"
        printf "%-60s %s\n" "--------" "-------"

        # Process URLs
        while IFS= read -r url; do
            [ -z "$url" ] && continue

            purpose=$(infer_purpose "$url")

            # Truncate long URLs for terminal display
            display_url="$url"
            if [ ${#url} -gt 58 ]; then
                display_url="${url:0:55}..."
            fi

            # Flag suspicious
            if echo "$purpose" | grep -qi "suspicious\|exfiltration"; then
                echo -e "${RED}${BOLD}$display_url${NC} ${RED}$purpose${NC}"
            else
                printf "%-60s %s\n" "$display_url" "$purpose"
            fi
        done <<< "$URLS"
    else
        success "No hardcoded URLs found in main source"
    fi

    subheader "Obfuscation Checks"

    # Check for eval in non-minified files
    EVAL_USAGE=$(grep -rn "eval(" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null | grep -v node_modules | grep -v "bundle\|vendor\|chunk" | head -3)
    if [ -n "$EVAL_USAGE" ]; then
        high "eval() usage found in source files"
    fi

    FUNC_CONSTR=$(grep -rn "new Function(" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null | grep -v node_modules | grep -v "bundle\|vendor\|chunk" | head -3)
    if [ -n "$FUNC_CONSTR" ]; then
        high "new Function() usage found (dynamic code generation)"
    fi

    subheader "Secrets Scan"

    AWS_KEYS=$(grep -rE "AKIA[0-9A-Z]{16}" "$EXTRACTED_PATH" 2>/dev/null | grep -v node_modules | head -3)
    if [ -n "$AWS_KEYS" ]; then
        critical "AWS Access Key found"
    fi

    GITHUB_TOKENS=$(grep -rE "gh[pousr]_[A-Za-z0-9]{36}" "$EXTRACTED_PATH" 2>/dev/null | grep -v node_modules | head -3)
    if [ -n "$GITHUB_TOKENS" ]; then
        critical "GitHub token found"
    fi

    STRIPE_KEYS=$(grep -rE "sk_live_[0-9a-zA-Z]{24}" "$EXTRACTED_PATH" 2>/dev/null | grep -v node_modules | head -3)
    if [ -n "$STRIPE_KEYS" ]; then
        critical "Stripe live key found"
    fi

    PRIVATE_KEYS=$(grep -rE "BEGIN (RSA|DSA|EC|OPENSSH) PRIVATE KEY" "$EXTRACTED_PATH" 2>/dev/null | grep -v node_modules | head -3)
    if [ -n "$PRIVATE_KEYS" ]; then
        critical "Private key found"
    fi

    subheader "Shell Execution"

    CHILD_PROCESS=$(grep -rn "require.*child_process\|execSync\|spawn(" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null | grep -v node_modules | wc -l | tr -d ' ')
    if [ "$CHILD_PROCESS" -gt 0 ]; then
        medium "Shell execution via child_process ($CHILD_PROCESS occurrences)"
    fi

    SHELL_OPEN=$(grep -rn "shell.openExternal\|shell.openPath" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null | grep -v node_modules | wc -l | tr -d ' ')
    if [ "$SHELL_OPEN" -gt 0 ]; then
        low "Electron shell API usage ($SHELL_OPEN occurrences)"
    fi

else
    # Binary analysis for non-Electron apps
    subheader "Binary String Analysis"

    BINARY_FILES=$(find "$BINARY_PATH" -type f -perm +111 2>/dev/null)
    for BINARY in $BINARY_FILES; do
        info "Analyzing binary: $(basename "$BINARY")"

        # Extract URLs from binary
        BINARY_URL_COUNT=$(strings -a "$BINARY" 2>/dev/null | grep -cE "https?://" || echo "0")
        if [ "$BINARY_URL_COUNT" -gt 0 ]; then
            info "Found $BINARY_URL_COUNT URLs in binary strings"
        fi

        # Check for suspicious strings
        SUSPICIOUS=$(strings -a "$BINARY" 2>/dev/null | grep -iE "(keylog|password.?stealer|backdoor)" | head -3)
        if [ -n "$SUSPICIOUS" ]; then
            critical "Suspicious strings in binary"
        fi
    done
fi

# =============================================================================
header "Phase 6: Electron Security Configuration"
# =============================================================================

if [ -n "$EXTRACTED_PATH" ] && [ -d "$EXTRACTED_PATH" ]; then
    # Critical: nodeIntegration
    if grep -rq "nodeIntegration:\s*true" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null; then
        critical "nodeIntegration: true (allows Node.js in renderer - major security risk)"
    else
        success "nodeIntegration appears disabled"
    fi

    # Critical: contextIsolation
    if grep -rq "contextIsolation:\s*false" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null; then
        critical "contextIsolation: false (preload scripts share context with page)"
    else
        success "contextIsolation appears enabled"
    fi

    # High: webSecurity
    if grep -rq "webSecurity:\s*false" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null; then
        high "webSecurity: false (same-origin policy disabled)"
    fi

    # High: sandbox
    if grep -rq "sandbox:\s*false" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null; then
        high "sandbox: false (renderer process not sandboxed)"
    fi

    # Medium: allowRunningInsecureContent
    if grep -rq "allowRunningInsecureContent:\s*true" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null; then
        medium "allowRunningInsecureContent: true"
    fi

    # Check for remote module (deprecated, insecure)
    if grep -rq "enableRemoteModule:\s*true" "$EXTRACTED_PATH" --include="*.js" 2>/dev/null; then
        medium "enableRemoteModule: true (deprecated, security risk)"
    fi

    # Check CSP
    CSP=$(grep -rh "Content-Security-Policy" "$EXTRACTED_PATH" --include="*.js" --include="*.html" 2>/dev/null | head -1)
    if [ -n "$CSP" ]; then
        success "Content-Security-Policy defined"
    else
        low "No Content-Security-Policy found"
    fi
fi

# =============================================================================
header "Phase 7: Persistence Mechanisms"
# =============================================================================

# Check for embedded LaunchAgents
LAUNCH_AGENTS=$(find "$APP_PATH" -name "*.plist" -exec grep -l "RunAtLoad\|KeepAlive\|ProgramArguments" {} \; 2>/dev/null)
if [ -n "$LAUNCH_AGENTS" ]; then
    high "Embedded LaunchAgent/Daemon plists found"
fi

# Check binary for persistence-related strings
BINARY_FILES=$(find "$BINARY_PATH" -type f -perm +111 2>/dev/null)
for BINARY in $BINARY_FILES; do
    PERSISTENCE=$(strings -a "$BINARY" 2>/dev/null | grep -iE "(LaunchAgent|LoginItem|LSSharedFileList|SMLoginItem)" | head -1)
    if [ -n "$PERSISTENCE" ]; then
        medium "Persistence mechanism indicators in binary"
        break
    fi
done

# Check for helper tools
if [ -d "$APP_PATH/Contents/Library/LaunchServices" ]; then
    medium "Contains LaunchServices helpers"
fi

if [ -d "$APP_PATH/Contents/XPCServices" ]; then
    info "Contains XPC Services"
fi

# =============================================================================
header "Summary"
# =============================================================================

echo ""
echo -e "${BOLD}Findings Summary:${NC}"
echo -e "  ${RED}${BOLD}CRITICAL:${NC} $CRITICAL_COUNT"
echo -e "  ${RED}HIGH:${NC}     $HIGH_COUNT"
echo -e "  ${YELLOW}MEDIUM:${NC}   $MEDIUM_COUNT"
echo -e "  ${CYAN}LOW:${NC}      $LOW_COUNT"
echo -e "  ${BLUE}INFO:${NC}     $INFO_COUNT"
echo ""

# Determine verdict
if [ $CRITICAL_COUNT -gt 0 ]; then
    echo -e "${RED}${BOLD}VERDICT: DO NOT RUN${NC}"
    echo "Critical security issues detected that require investigation."
elif [ $HIGH_COUNT -gt 0 ]; then
    echo -e "${YELLOW}${BOLD}VERDICT: REVIEW RECOMMENDED${NC}"
    echo "High-severity findings detected. Review before granting sensitive permissions."
elif [ $MEDIUM_COUNT -gt 0 ]; then
    echo -e "${CYAN}${BOLD}VERDICT: MODERATE RISK${NC}"
    echo "Some concerns identified. Review if handling sensitive data."
else
    echo -e "${GREEN}${BOLD}VERDICT: LOW RISK${NC}"
    echo "No significant security concerns detected."
fi

echo ""
echo -e "${BOLD}Note:${NC} This scan cannot detect obfuscated code, delayed payloads, or server-side changes."

# Cleanup temp files
if [ -d "/tmp/audit-$$" ]; then
    rm -rf "/tmp/audit-$$"
fi
