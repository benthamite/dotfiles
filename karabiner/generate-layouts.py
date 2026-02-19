#!/usr/bin/env python3
"""Generate keyboard layout SVGs from Karabiner simlayer config.

Parses modifications.org to extract simlayer rules, converts each to
keymap-drawer YAML, and runs `keymap draw` to produce SVGs.
"""

import os
import re
import subprocess
import shutil

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
MODIFICATIONS_ORG = os.path.join(SCRIPT_DIR, "modifications.org")
LAYOUTS_DIR = os.path.join(SCRIPT_DIR, "layouts")
HUGO_IMG_DIR = os.path.join(
    os.path.expanduser("~"),
    "Library/CloudStorage/Dropbox/repos/stafforini.com/static/images/keyboard",
)

# The 36 physical positions of the Corne 3x5+3 layout, in the order
# keymap-drawer expects (left-to-right, top-to-bottom, then thumbs).
CORNE_KEYS = [
    "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
    "a", "s", "d", "f", "g", "h", "j", "k", "l", "semicolon",
    "z", "x", "c", "v", "b", "n", "m", "comma", "period", "slash",
    "left_control", "left_option", "left_command",
    "spacebar", "right_command", "right_option",
]

KEY_DISPLAY = {
    "q": "Q", "w": "W", "e": "E", "r": "R", "t": "T",
    "y": "Y", "u": "U", "i": "I", "o": "O", "p": "P",
    "a": "A", "s": "S", "d": "D", "f": "F", "g": "G",
    "h": "H", "j": "J", "k": "K", "l": "L", "semicolon": ";",
    "z": "Z", "x": "X", "c": "C", "v": "V", "b": "B",
    "n": "N", "m": "M", "comma": ",", "period": ".", "slash": "/",
    "left_control": "Esc", "left_option": "Enter", "left_command": "Win",
    "spacebar": "Space", "right_command": "App", "right_option": "Tab",
    "return_or_enter": "Enter", "tab": "Tab",
    "delete_or_backspace": "Bksp", "delete_forward": "Del",
    "left_arrow": "Left", "right_arrow": "Right",
    "up_arrow": "Up", "down_arrow": "Down",
    "page_up": "PgUp", "page_down": "PgDn",
    "home": "Home", "end": "End",
    "play_or_pause": "Play", "rewind": "Rew", "fast_forward": "FF",
    "volume_increment": "Vol+", "volume_decrement": "Vol-", "mute": "Mute",
    "button1": "Click", "button2": "RClick",
    "equal_sign": "=", "hyphen": "-",
    "open_bracket": "[", "close_bracket": "]",
    "backslash": "\\", "quote": "'",
    "grave_accent_and_tilde": "`",
}

SIMLAYER_TRIGGERS = {
    "b-mode": "b", "f-mode": "f", "j-mode": "j", "k-mode": "k",
    "m-mode": "m", "p-mode": "p", "q-mode": "q", "v-mode": "v",
    "x-mode": "x", "y-mode": "y", "z-mode": "z",
    "comma-mode": "comma", "period-mode": "period",
    "semicolon-mode": "semicolon", "slash-mode": "slash",
}

# Simlayers that only activate in Emacs (have :condi :emacs)
EMACS_ONLY = {"x-mode", "comma-mode"}

# :tos character aliases -> display character
TOS_CHARS = {
    # special chars (non-ASCII tos names)
    "ð": "ð", "…": "...", "¡": "!", "ø": "ø", "£": "£", "œ": "œ",
    "€": "€", "ß": "ß", "þ": "þ", "•": "•", "¿": "?", "«": "«", "»": "»",
    # math (non-ASCII tos names)
    "=": "=", "+": "+", "≠": "≠", "÷": "÷", "±": "±", "≤": "≤", "≥": "≥",
    # diacritics (ASCII tos names)
    "macron": "Macron", "breve": "Breve", "cedilla": "Cedilla",
    "accute_accent": "Acute", "undercomma": "Comma", "underbar": "Bar",
    "horn": "Horn", "double_acute_accent": "Dbl acute", "overring": "Ring",
    "stroke": "Stroke", "ogonek": "Ogonek", "tilde_accent": "Tilde",
    "grave_accent": "Grave", "umlaut": "Umlaut", "caron": "Caron",
    "underdot": "Underdot", "overdot": "Overdot", "circumflex": "Circ",
    "hook": "Hook",
    # symbols (ASCII tos names)
    "exclamation_mark": "!", "at_sign": "@", "number_sign": "#",
    "dollar_sign": "$", "percent_sign": "%", "caret": "^",
    "ampersand": "&", "asterisk": "*", "open_parenthesis": "(",
    "close_parenthesis": ")", "underscore": "_", "plus_sign": "+",
    "open_brace": "{", "close_brace": "}", "vertical_bar": "|",
    "double_quote": '"', "tilde": "~",
    "open_double_quote": "\u201c", "close_double_quote": "\u201d",
    "open_single_quote": "\u2018", "close_single_quote": "\u2019",
    "en_dash": "\u2013", "em_dash": "\u2014",
    # media (ASCII tos names)
    "rewind": "Rew", "play_or_pause": "Play", "fast_forward": "FF",
    "volume_increment": "Vol+", "volume_decrement": "Vol-", "mute": "Mute",
}

# Explicit app name overrides for long/awkward names
APP_NAME_OVERRIDES = {
    "Karabiner-Elements": "Karabiner",
    "GoldenDict-ng": "GoldenDict",
    "Google Chrome": "Chrome",
    "Plex Media Server": "Plex",
    "Beeper Desktop": "Beeper",
    "Home Assistant": "Home",
    "Keyboard Maestro": "KM",
    "Media Center 29": "Media Ctr",
    "Tor Browser": "Tor",
    "zoom.us": "Zoom",
}


def parse_simlayer_blocks(org_path):
    """Extract simlayer rule blocks from the org file."""
    with open(org_path, "r") as f:
        content = f.read()

    blocks = {}
    pattern = re.compile(
        r'\{:des\s+"[^"]*"\s*\n\s*:rules\s+\[(:[\w-]+-mode)\s*\n'
        r'(.*?)\]\s*\}',
        re.DOTALL,
    )
    for match in pattern.finditer(content):
        simlayer_name = match.group(1).lstrip(":")
        rules_text = match.group(2)
        rules = []
        for line in rules_text.split("\n"):
            line = line.strip()
            if line.startswith("["):
                rules.append(line)
        blocks[simlayer_name] = rules

    return blocks


def parse_rule(rule_str):
    """Parse a single Clojure rule vector into components.

    Returns (trigger_key, action, condition, comment) or None.
    """
    # Extract trailing comment (Clojure ; comment)
    comment = None
    # Match semicolon-as-comment only when preceded by ] or whitespace
    # to avoid matching :semicolon inside the rule
    comment_match = re.search(r'\]\s*;\s*(.+)$', rule_str)
    if comment_match:
        comment = comment_match.group(1).strip()
        # Remove just the comment part, keep the closing ]
        rule_str = rule_str[:comment_match.start() + 1].rstrip()

    # Remove outer brackets
    rule_str = rule_str.strip()
    if not rule_str.startswith("[") or not rule_str.endswith("]"):
        return None
    inner = rule_str[1:-1].strip()

    # Parse the trigger key (first element): :key_name
    # Handle :word_chars, :## prefixed, and :! prefixed
    trigger_match = re.match(r':(##|![\w]*)?(\w+)', inner)
    if not trigger_match:
        return None

    prefix = trigger_match.group(1) or ""
    trigger_key = trigger_match.group(2)

    # Skip modifier-variant rules (! prefixed = Shift/Ctrl variants)
    if prefix.startswith("!"):
        return None

    # Mark ## prefix so caller can deprioritize it vs plain rules
    is_hash = prefix == "##"

    rest = inner[trigger_match.end():].strip()

    # Parse condition (last element: :emacs, :!emacs, :chrome, etc.)
    condition = None
    cond_match = re.search(
        r'\s+:((?:!)?(?:emacs|chrome|firefox|safari|anki))\s*$', rest
    )
    if cond_match:
        condition = cond_match.group(1)
        rest = rest[:cond_match.start()].strip()

    action = parse_action(rest)
    return trigger_key, action, condition, comment, is_hash


def parse_action(action_str):
    """Parse the action part of a rule."""
    action_str = action_str.strip()

    # Template: [:insert "X"]
    insert_match = re.match(r'\[:insert\s+"([^"]+)"\]', action_str)
    if insert_match:
        return {"type": "insert", "char": insert_match.group(1)}

    # Template: [:open "..."]
    open_match = re.match(r'\[:open\s+"([^"]+)"\]', action_str)
    if open_match:
        return {"type": "open", "app": extract_app_name(open_match.group(1))}

    # Template: [:km "..."]
    km_match = re.match(r'\[:km\s+"([^"]+)"\]', action_str)
    if km_match:
        return {"type": "km", "name": km_match.group(1)}

    # Mouse key: {:mkey {:x N}} or {:mkey {:y N}}
    mkey_match = re.match(r'\{:mkey\s+\{:([xy])\s+(-?\d+)\}\}', action_str)
    if mkey_match:
        return {
            "type": "mkey",
            "axis": mkey_match.group(1),
            "value": int(mkey_match.group(2)),
        }

    # Multi-key sequence: [{...} {...}] (must check before single modi)
    seq_match = re.match(r'\[\{:modi\s+.*\}\s+\{:modi\s+.*\}\]', action_str)
    if seq_match:
        return {"type": "sequence"}

    # Modifier combo wrapped in vector: [{:modi :X :key :Y}]
    vec_modi_match = re.match(
        r'\[\{:modi\s+:([^\s]+)\s+:key\s+:([^\s}]+)\}\]', action_str
    )
    if vec_modi_match:
        return {
            "type": "modi",
            "modi": vec_modi_match.group(1),
            "key": vec_modi_match.group(2),
        }

    # Modifier combo: {:modi :X :key :Y}
    modi_match = re.match(
        r'\{:modi\s+:([^\s]+)\s+:key\s+:([^\s}]+)\}', action_str
    )
    if modi_match:
        return {
            "type": "modi",
            "modi": modi_match.group(1),
            "key": modi_match.group(2),
        }

    # Simple key reference: :key_name (including non-ASCII like :=, :≠)
    # Match : followed by one or more non-whitespace, non-bracket chars
    # Strip ## prefix from action side (modifier passthrough marker)
    key_match = re.match(r':(?:##)?([^\s\[\]{}]+)', action_str)
    if key_match:
        key_name = key_match.group(1)
        # Check tos char aliases first
        if key_name in TOS_CHARS:
            return {"type": "char", "char": TOS_CHARS[key_name]}
        # Check well-known keys
        if key_name in KEY_DISPLAY:
            return {"type": "key", "key": key_name}
        # Number keys
        if re.match(r'^\d$', key_name):
            return {"type": "char", "char": key_name}
        # Mouse buttons
        if key_name.startswith("button"):
            return {"type": "button", "button": key_name}
        # Fallback
        return {"type": "key", "key": key_name}

    return {"type": "unknown", "raw": action_str}


def extract_app_name(path):
    """Extract short application name from a macOS .app path."""
    basename = os.path.basename(path.rstrip("/"))
    name = basename.replace(".app", "")
    # Check overrides
    if name in APP_NAME_OVERRIDES:
        return APP_NAME_OVERRIDES[name]
    # Shorten if too long
    if len(name) > 12:
        # Take first word
        first = name.split()[0] if " " in name else name.split("-")[0]
        return first
    return name


def shorten_comment(comment):
    """Shorten a Clojure comment to a useful label."""
    # Remove trailing context like "(Settings > ...)" or "(alias for ...)"
    comment = re.sub(r'\s*\(.*\)\s*$', '', comment)
    # Remove common Elisp prefixes
    for prefix in ("frame-extras-", "ps-extras-"):
        if comment.startswith(prefix):
            comment = comment[len(prefix):]
    # For "App, action" patterns, take the action part
    if ", " in comment:
        parts = comment.split(", ", 1)
        # Use the action part if short enough, otherwise the app part
        comment = parts[1] if len(parts[1]) <= 12 else parts[0]
    # Convert kebab-case to spaces for readability
    if "-" in comment and " " not in comment:
        comment = comment.replace("-", " ")
    # Capitalize first letter
    if comment:
        comment = comment[0].upper() + comment[1:]
    # Truncate at word boundary
    if len(comment) > 12:
        words = comment.split()
        truncated = ""
        for word in words:
            if len(truncated) + len(word) + 1 > 12:
                break
            truncated = f"{truncated} {word}" if truncated else word
        comment = truncated or words[0][:12]
    return comment


def make_label(action, comment=None):
    """Convert a parsed action to a display label for the SVG."""
    if action is None:
        return None

    atype = action["type"]

    if atype in ("char", "insert"):
        return action["char"]

    if atype == "open":
        return action["app"]

    if atype == "km":
        name = action["name"]
        if name.startswith("Local "):
            return "KM"
        if name.startswith("open: "):
            short = name[6:]
            return short[:10] if len(short) > 10 else short
        return "KM"

    if atype == "key":
        return KEY_DISPLAY.get(action["key"], action["key"])

    if atype == "modi":
        if comment:
            return shorten_comment(comment)
        modi = action["modi"]
        key = action["key"]
        key_label = KEY_DISPLAY.get(key, key.upper() if len(key) == 1 else key)
        return f"{modi}{key_label}"

    if atype == "mkey":
        axis = action["axis"]
        value = action["value"]
        if axis == "x":
            d = "Left" if value < 0 else "Right"
        else:
            d = "Up" if value < 0 else "Down"
        suffix = "++" if abs(value) > 1500 else ""
        return f"{d}{suffix}"

    if atype == "button":
        return KEY_DISPLAY.get(action["button"], action["button"])

    if atype == "sequence":
        return "Seq"

    return "?"


def build_layer_keys(simlayer_name, rules):
    """Build a 36-element key list for keymap-drawer YAML."""
    key_labels = {}
    hash_labels = {}  # ## prefixed rules (lower priority)
    for rule_str in rules:
        parsed = parse_rule(rule_str)
        if parsed is None:
            continue
        trigger_key, action, condition, comment, is_hash = parsed

        # Skip non-Emacs fallbacks if we already have a primary rule
        if condition == "!emacs" and trigger_key in key_labels:
            continue
        # Skip app-specific rules (Chrome, etc.)
        if condition in ("chrome", "firefox", "safari"):
            continue

        if is_hash:
            # Store ## rules as fallbacks (used if no plain rule exists)
            if trigger_key not in hash_labels:
                label = make_label(action, comment)
                if label:
                    hash_labels[trigger_key] = label
        elif trigger_key not in key_labels:
            label = make_label(action, comment)
            if label:
                key_labels[trigger_key] = label

    # Merge: plain rules take priority, ## rules fill gaps
    for k, v in hash_labels.items():
        if k not in key_labels:
            key_labels[k] = v

    # Build the 36-element list
    trigger = SIMLAYER_TRIGGERS.get(simlayer_name)
    result = []
    for pos_key in CORNE_KEYS:
        if pos_key == trigger:
            result.append({"type": "held"})
        elif pos_key in key_labels:
            result.append(key_labels[pos_key])
        else:
            result.append(None)

    return result


def build_base_layer_keys():
    """Build the base layer showing simlayer hold annotations."""
    simlayer_holds = {
        "q": "apps", "f": "chars", "j": "del", "k": "site",
        "m": "math", "p": "diacrit", "v": "numbers", "b": "media",
        "x": "avy", "y": "mouse", "z": "nav",
        "comma": "transp", "period": "manip",
        "semicolon": "sym", "slash": "org/app",
    }
    thumb_info = {
        "left_control": {"t": "Esc", "h": "Meta"},
        "left_option": {"t": "Enter", "h": "Shift"},
        "left_command": {"t": "Win", "h": "Hyper"},
        "spacebar": {"t": "Space", "h": "Ctrl"},
        "right_command": {"t": "App", "h": "Super"},
        "right_option": {"t": "Tab", "h": "Alt"},
    }

    result = []
    for pos_key in CORNE_KEYS:
        if pos_key in simlayer_holds:
            base_label = KEY_DISPLAY.get(pos_key, pos_key.upper())
            result.append({"t": base_label, "h": simlayer_holds[pos_key]})
        elif pos_key in thumb_info:
            result.append(thumb_info[pos_key])
        else:
            result.append(KEY_DISPLAY.get(pos_key, pos_key.upper()))

    return result


def format_yaml_key(key):
    """Format a single key for YAML output."""
    if key is None:
        return "null"
    if isinstance(key, dict):
        if key.get("type") == "held":
            return "{type: held}"
        parts = []
        if "t" in key:
            parts.append(f't: "{key["t"]}"')
        if "h" in key:
            parts.append(f'h: "{key["h"]}"')
        return "{" + ", ".join(parts) + "}"
    if isinstance(key, str):
        # Escape backslashes and double quotes for YAML double-quoted strings
        escaped = key.replace("\\", "\\\\").replace('"', '\\"')
        return f'"{escaped}"'
    return str(key)


def build_yaml_content(layer_name, keys):
    """Build keymap-drawer YAML for a single layer."""
    rows = []
    for start in (0, 10, 20):
        row = [format_yaml_key(k) for k in keys[start:start + 10]]
        rows.append(f"    - [{', '.join(row)}]")
    for k in keys[30:36]:
        rows.append(f"    - {format_yaml_key(k)}")

    layer_rows = "\n".join(rows)
    return f"""layout:
  qmk_keyboard: corne_rotated
  layout_name: LAYOUT_split_3x5_3
layers:
  {layer_name}:
{layer_rows}
"""


def write_yaml(layer_name, yaml_content, output_dir):
    """Write YAML to file."""
    os.makedirs(output_dir, exist_ok=True)
    path = os.path.join(output_dir, f"{layer_name}.yaml")
    with open(path, "w") as f:
        f.write(yaml_content)
    return path


def run_keymap_draw(yaml_path, svg_path):
    """Run keymap draw to produce SVG."""
    result = subprocess.run(
        ["keymap", "draw", yaml_path, "-o", svg_path],
        capture_output=True, text=True,
    )
    if result.returncode != 0:
        print(f"  ERROR: keymap draw failed for {yaml_path}")
        print(f"  stderr: {result.stderr}")
        return False
    return True


def main():
    print("Parsing modifications.org...")
    blocks = parse_simlayer_blocks(MODIFICATIONS_ORG)
    print(f"Found {len(blocks)} simlayer blocks: {', '.join(sorted(blocks.keys()))}")

    os.makedirs(LAYOUTS_DIR, exist_ok=True)

    # Generate base layer
    print("\nGenerating base layer...")
    base_keys = build_base_layer_keys()
    base_yaml = build_yaml_content("base", base_keys)
    yaml_path = write_yaml("base", base_yaml, LAYOUTS_DIR)
    svg_path = os.path.join(LAYOUTS_DIR, "base.svg")
    if run_keymap_draw(yaml_path, svg_path):
        print(f"  -> {svg_path}")

    # Generate simlayer SVGs
    for simlayer_name in sorted(blocks.keys()):
        print(f"\nGenerating {simlayer_name}...")
        rules = blocks[simlayer_name]
        keys = build_layer_keys(simlayer_name, rules)
        display_name = (
            f"{simlayer_name} (Emacs only)"
            if simlayer_name in EMACS_ONLY
            else simlayer_name
        )
        yaml_content = build_yaml_content(display_name, keys)
        yaml_path = write_yaml(simlayer_name, yaml_content, LAYOUTS_DIR)
        svg_path = os.path.join(LAYOUTS_DIR, f"{simlayer_name}.svg")
        if run_keymap_draw(yaml_path, svg_path):
            print(f"  -> {svg_path}")

    # Copy SVGs to Hugo static dir
    print(f"\nCopying SVGs to {HUGO_IMG_DIR}...")
    os.makedirs(HUGO_IMG_DIR, exist_ok=True)
    svg_count = 0
    for fname in sorted(os.listdir(LAYOUTS_DIR)):
        if fname.endswith(".svg"):
            src = os.path.join(LAYOUTS_DIR, fname)
            dst = os.path.join(HUGO_IMG_DIR, fname)
            shutil.copy2(src, dst)
            svg_count += 1
    print(f"Copied {svg_count} SVGs.")

    print("\nDone!")


if __name__ == "__main__":
    main()
