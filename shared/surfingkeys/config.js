// A very tridactyl-esque config file.

// ==========================================================
// 1. API DESTRUCTURING
// ==========================================================
const {
  Hints,
  Front,
  Visual,
  Clipboard,
  map,
  mapkey,
  unmap,
  iunmap,
  addSearchAlias,
  removeSearchAlias,
  tabOpenLink,
} = api;

// ==========================================================
// 2. BROWSER DETECTION & SETTINGS
// ==========================================================
const isFirefox = navigator.userAgent.includes("Firefox");
const isEdge = navigator.userAgent.includes("Edg");

Hints.setCharacters("asdfgyuiopqwertnmzxcvb");
settings.defaultSearchEngine = "g";
settings.hintAlign = "left";
settings.focusFirstCandidate = false;
settings.focusAfterClosed = "last";
settings.scrollStepSize = 200;
settings.tabsThreshold = 7;
settings.modeAfterYank = "Normal";

// ==========================================================
// 3. MAP / REMAP
// ==========================================================
map("P", "p"); // Passthrough
map("p", "cc"); // Paste link / open clipboard
map("gf", "w");
map("`", "'");
map("o", "go");

map("H", "S"); // History Back
map("L", "D"); // History Forward
map("D", "x"); // Tab Delete
map("<Alt-j>", "R"); // Tab Next
map("<Alt-k>", "E"); // Tab Prev

map("]", "]]"); // Next Page
map("[", "[["); // Prev Page
map("K", "[[");
map("J", "]]");

map(">", ">>"); // Move Tab Right
map("<", "<<"); // Move Tab Left

// ==========================================================
// 4. CROSS-BROWSER INTERNAL PAGES (TODO: Broken)
// ==========================================================
mapkey("si", "#12Open Browser Inspect/Debugging", function () {
  let targetUrl = "chrome://inspect/";
  if (isFirefox) targetUrl = "about:debugging";
  else if (isEdge) targetUrl = "edge://inspect/";
  tabOpenLink(targetUrl);
});

mapkey("gc", "#12Open Browser Extensions", function () {
  let targetUrl = "chrome://extensions/";
  if (isFirefox) targetUrl = "about:addons";
  else if (isEdge) targetUrl = "edge://extensions/";
  tabOpenLink(targetUrl);
});

mapkey("gs", "#12Open Browser Settings", function () {
  let targetUrl = "chrome://settings/";
  if (isFirefox) targetUrl = "about:preferences";
  else if (isEdge) targetUrl = "edge://settings/";
  tabOpenLink(targetUrl);
});

mapkey("gF", "#12Open Browser Flags/Config", function () {
  let targetUrl = "chrome://flags/";
  if (isFirefox) targetUrl = "about:config";
  else if (isEdge) targetUrl = "edge://flags/";
  tabOpenLink(targetUrl);
});

// ==========================================================
// 5. SEARCH ENGINES & ALIASES
// ==========================================================
removeSearchAlias("b", "s"); // baidu
removeSearchAlias("y", "s"); // yandex

addSearchAlias("gh", "github", "https://github.com/search?utf8=✓&q=", "s");
addSearchAlias("r", "reddit", "https://old.reddit.com/r/", "s");
addSearchAlias(
  "w",
  "wikipedia",
  "https://en.wikipedia.org/wiki/Special:Search/",
  "s",
);
addSearchAlias(
  "n",
  "nixos",
  "https://search.nixos.org/packages?channel=unstable&query=",
  "s",
);
addSearchAlias("hg", "hoogle", "https://hoogle.haskell.org/?hoogle=", "s");

// ==========================================================
// 7. VIM-STYLE JUMP LIST (Position-Aware Rewind)
// ==========================================================
unmap("<Ctrl-i>");
unmap("<Ctrl-o>");

let markStack = [];
let markIndex = -1;

function saveMarkXY(x, y) {
  if (markIndex < markStack.length - 1)
    markStack = markStack.slice(0, markIndex + 1);
  if (markStack.length > 0) {
    let last = markStack[markStack.length - 1];
    if (last.x === x && last.y === y) return;
  }
  markStack.push({ x: x, y: y });
  markIndex = markStack.length - 1;
}

function saveMark() {
  saveMarkXY(window.scrollX, window.scrollY);
}

mapkey("gg", "#2Scroll to the top of the page", function () {
  saveMark();
  window.scrollTo({ top: 0, behavior: "smooth" });
  saveMarkXY(0, 0);
});

mapkey("G", "#2Scroll to the bottom of the page", function () {
  saveMark();
  window.scrollTo({ top: document.body.scrollHeight, behavior: "smooth" });
  saveMarkXY(0, document.body.scrollHeight);
});

mapkey("<Ctrl-o>", "#2Jump back in position/history", function () {
  if (markIndex > 0) {
    markIndex--;
    window.scrollTo({
      top: markStack[markIndex].y,
      left: markStack[markIndex].x,
      behavior: "smooth",
    });
    Front.showBanner(`Jumped back (${markIndex + 1}/${markStack.length})`);
  } else history.go(-1);
});

mapkey("<Ctrl-i>", "#2Jump forward in position/history", function () {
  if (markIndex < markStack.length - 1) {
    markIndex++;
    window.scrollTo({
      top: markStack[markIndex].y,
      left: markStack[markIndex].x,
      behavior: "smooth",
    });
    Front.showBanner(`Jumped forward (${markIndex + 1}/${markStack.length})`);
  } else history.go(1);
});

// ==========================================================
// 8. UNMAPS
// ==========================================================
unmap("spa");
unmap("spb");
unmap("spc");
unmap("spd");
unmap("sps");
unmap("cp");
unmap(";cp");
unmap(";ap");
iunmap(":");
unmap(";t");
unmap("ga");
unmap("gr");

// ==========================================================
// 9. DYNAMIC THEME MANAGER (Omnibar + DB API + Pure CSS)
// ==========================================================
const baseCSS = `
  .sk_theme { background: var(--bg); color: var(--fg); border-color: var(--border); font-family: var(--font); font-size: var(--font-size); font-weight: var(--font-weight); }
  input { font-family: var(--font); font-weight: var(--font-weight); }
  .sk_theme tbody { color: var(--fg); }
  .sk_theme input { color: var(--fg); }

  #sk_tabs .sk_tab { background: var(--bg-dark); border: 1px solid var(--border); }
  #sk_tabs .sk_tab_title { color: var(--fg); }
  #sk_tabs .sk_tab_url { color: var(--main-fg); }
  #sk_tabs .sk_tab_hint { background: var(--bg); border: 1px solid var(--border); color: var(--accent-fg); }
  .sk_theme #sk_frame { background: var(--bg); opacity: 0.2; color: var(--accent-fg); }
  .sk_theme .title { color: var(--accent-fg); }
  .sk_theme .url { color: var(--main-fg); }
  .sk_theme .annotation { color: var(--accent-fg); }
  .sk_theme .omnibar_highlight { color: var(--accent-fg); }
  .sk_theme .omnibar_timestamp { color: var(--info-fg); }
  .sk_theme .omnibar_visitcount { color: var(--accent-fg); }
  .sk_theme #sk_omnibarSearchResult ul li:nth-child(odd) { background: var(--bg-dark); }
  .sk_theme #sk_omnibarSearchResult ul li.focused { background: var(--select); }
  .sk_theme #sk_omnibarSearchArea { border-top-color: var(--border); border-bottom-color: var(--border); }
  .sk_theme #sk_omnibarSearchArea input, .sk_theme #sk_omnibarSearchArea span { font-size: var(--font-size); }
  .sk_theme .separator { color: var(--accent-fg); }

  #sk_banner { font-family: var(--font); font-size: var(--font-size); font-weight: var(--font-weight); background: var(--bg); border-color: var(--border); color: var(--fg); opacity: 0.9; }
  #sk_keystroke { background-color: var(--bg); }
  .sk_theme kbd .candidates { color: var(--info-fg); }
  .sk_theme span.annotation { color: var(--accent-fg); }
  #sk_bubble { background-color: var(--bg) !important; color: var(--fg) !important; border-color: var(--border) !important; }
  #sk_bubble * { color: var(--fg) !important; }
  #sk_bubble div.sk_arrow div:nth-of-type(1) { border-top-color: var(--border) !important; border-bottom-color: var(--border) !important; }
  #sk_bubble div.sk_arrow div:nth-of-type(2) { border-top-color: var(--bg) !important; border-bottom-color: var(--bg) !important; }
  #sk_status, #sk_find { font-size: var(--font-size); border-color: var(--border); }
  .sk_theme kbd { background: var(--bg-dark); border-color: var(--border); box-shadow: none; color: var(--fg); }
  .sk_theme .feature_name span { color: var(--main-fg); }
  #sk_hints .begin { color: var(--main-fg) !important; }

  /* Editor Mode */
  #sk_editor { background: var(--bg-dark) !important; height: 50% !important; }
  .ace_dialog-bottom { border-top: 1px solid var(--bg) !important; }
  .ace-chrome .ace_print-margin, .ace_gutter, .ace_gutter-cell, .ace_dialog { background: var(--bg) !important; }
  .ace-chrome { color: var(--fg) !important; }
  .ace_gutter, .ace_dialog { color: var(--fg) !important; }
  .ace_cursor { color: var(--fg) !important; }
  .normal-mode .ace_cursor { background-color: var(--accent-fg) !important; border: var(--accent-fg) !important; opacity: 0.7 !important; }
  .ace_marker-layer .ace_selection { background: var(--select) !important; }
`;

const themes = {
  dracula: {
    name: "Dracula (Dark)",
    hints:
      "background: #282A36 !important; background-image: none !important; border: solid 2px #6272A4 !important; color: #50FA7B !important;",
    textHints:
      "background: #282A36 !important; background-image: none !important; border: solid 2px #6272A4 !important; color: #FF79C6 !important; padding: 1px !important;",
    visualMarks: "background-color: #50FA7B99;",
    visualCursor: "background-color: #8BE9FD;",
    vars: {
      "--font": "'Source Code Pro', Ubuntu, sans",
      "--font-size": "12pt",
      "--font-weight": "bold",
      "--fg": "#F8F8F2",
      "--bg": "#282A36",
      "--bg-dark": "#1E1F29",
      "--border": "#6272A4",
      "--main-fg": "#8BE9FD",
      "--accent-fg": "#50FA7B",
      "--info-fg": "#FF79C6",
      "--select": "#44475A",
    },
  },
  nord: {
    name: "Nord (Dark)",
    hints:
      "background: #2E3440 !important; background-image: none !important; border: solid 2px #4C566A !important; color: #A3BE8C !important;",
    textHints:
      "background: #2E3440 !important; background-image: none !important; border: solid 2px #4C566A !important; color: #88C0D0 !important; padding: 1px !important;",
    visualMarks: "background-color: #A3BE8C99;",
    visualCursor: "background-color: #88C0D0;",
    vars: {
      "--font": "'Source Code Pro', Ubuntu, sans",
      "--font-size": "12pt",
      "--font-weight": "bold",
      "--fg": "#D8DEE9",
      "--bg": "#2E3440",
      "--bg-dark": "#242933",
      "--border": "#4C566A",
      "--main-fg": "#88C0D0",
      "--accent-fg": "#A3BE8C",
      "--info-fg": "#B48EAD",
      "--select": "#434C5E",
    },
  },
  gruvbox: {
    name: "Gruvbox (Dark)",
    hints:
      "background: #282828 !important; background-image: none !important; border: solid 2px #504945 !important; color: #B8BB26 !important;",
    textHints:
      "background: #282828 !important; background-image: none !important; border: solid 2px #504945 !important; color: #83A598 !important; padding: 1px !important;",
    visualMarks: "background-color: #B8BB2699;",
    visualCursor: "background-color: #83A598;",
    vars: {
      "--font": "'Source Code Pro', Ubuntu, sans",
      "--font-size": "12pt",
      "--font-weight": "bold",
      "--fg": "#EBDBB2",
      "--bg": "#282828",
      "--bg-dark": "#1D2021",
      "--border": "#504945",
      "--main-fg": "#83A598",
      "--accent-fg": "#B8BB26",
      "--info-fg": "#D3869B",
      "--select": "#3C3836",
    },
  },
  solarizedDark: {
    name: "Solarized (Dark)",
    hints:
      "background: #002B36 !important; background-image: none !important; border: solid 2px #586E75 !important; color: #859900 !important;",
    textHints:
      "background: #002B36 !important; background-image: none !important; border: solid 2px #586E75 !important; color: #B58900 !important; padding: 1px !important;",
    visualMarks: "background-color: #85990099;",
    visualCursor: "background-color: #268BD2;",
    vars: {
      "--font": "'Source Code Pro', Ubuntu, sans",
      "--font-size": "12pt",
      "--font-weight": "bold",
      "--fg": "#839496",
      "--bg": "#002B36",
      "--bg-dark": "#073642",
      "--border": "#586E75",
      "--main-fg": "#268BD2",
      "--accent-fg": "#859900",
      "--info-fg": "#B58900",
      "--select": "#586E75",
    },
  },
  githubLight: {
    name: "GitHub (Light)",
    hints:
      "background: #FFFFFF !important; background-image: none !important; border: solid 2px #D0D7DE !important; color: #1A7F37 !important;",
    textHints:
      "background: #FFFFFF !important; background-image: none !important; border: solid 2px #D0D7DE !important; color: #0969DA !important; padding: 1px !important;",
    visualMarks: "background-color: #1A7F3799;",
    visualCursor: "background-color: #0969DA;",
    vars: {
      "--font": "'Source Code Pro', Ubuntu, sans",
      "--font-size": "12pt",
      "--font-weight": "bold",
      "--fg": "#24292F",
      "--bg": "#FFFFFF",
      "--bg-dark": "#F6F8FA",
      "--border": "#D0D7DE",
      "--main-fg": "#0969DA",
      "--accent-fg": "#1A7F37",
      "--info-fg": "#8250DF",
      "--select": "#EAECEF",
    },
  },
  solarizedLight: {
    name: "Solarized (Light)",
    hints:
      "background: #FDF6E3 !important; background-image: none !important; border: solid 2px #93A1A1 !important; color: #859900 !important;",
    textHints:
      "background: #FDF6E3 !important; background-image: none !important; border: solid 2px #93A1A1 !important; color: #268BD2 !important; padding: 1px !important;",
    visualMarks: "background-color: #85990099;",
    visualCursor: "background-color: #268BD2;",
    vars: {
      "--font": "'Source Code Pro', Ubuntu, sans",
      "--font-size": "12pt",
      "--font-weight": "bold",
      "--fg": "#657B83",
      "--bg": "#FDF6E3",
      "--bg-dark": "#EEE8D5",
      "--border": "#93A1A1",
      "--main-fg": "#268BD2",
      "--accent-fg": "#859900",
      "--info-fg": "#D33682",
      "--select": "#E8DFCC",
    },
  },
};

let activeThemeKey = "";

function applyTheme(themeKey, saveGlobally = false) {
  let t = themes[themeKey];
  if (!t) return;
  activeThemeKey = themeKey;

  // 1. Live Repaint Current Tab Instantly (UI variables)
  for (const [key, value] of Object.entries(t.vars)) {
    document.documentElement.style.setProperty(key, value);
  }

  // 2. Override the Hints native API (Kills Gradients perfectly)
  Hints.style(t.hints);
  Hints.style(t.textHints, "text");
  Visual.style("marks", t.visualMarks);
  Visual.style("cursor", t.visualCursor);

  // 3. Build the CSS string for Surfingkeys settings
  let varsStr = Object.entries(t.vars)
    .map(([k, v]) => `${k}: ${v};`)
    .join(" ");
  settings.theme = `:root, .sk_theme { ${varsStr} }\n` + baseCSS;

  // 4. Save to database using the verified async hook
  if (saveGlobally) {
    api.RUNTIME("updateSettings", {
      settings: {
        sk_active_theme: themeKey,
        theme: settings.theme,
      },
    });
    Front.showBanner(`✅ Theme updated to ${t.name}`);
  }
}

// ----------------------------------------------------------
// Hash Change Interceptor (Catches Omnibar Selection)
// ----------------------------------------------------------
function handleThemeHash() {
  if (window.location.hash.startsWith("#sk_theme_")) {
    let themeKey = window.location.hash.replace("#sk_theme_", "");
    if (themes[themeKey]) {
      let scrollX = window.scrollX;
      let scrollY = window.scrollY;

      // Instantly apply AND save globally
      applyTheme(themeKey, true);

      // Silently wipe the hash from the URL
      let cleanUrl = window.location.href.split("#sk_theme_")[0];
      history.replaceState(null, null, cleanUrl);
      window.scrollTo(scrollX, scrollY);
    }
  }
}
window.addEventListener("hashchange", handleThemeHash);

// ----------------------------------------------------------
// Initialization on Page Load
// ----------------------------------------------------------
if (window.location.hash.startsWith("#sk_theme_")) {
  handleThemeHash();
} else {
  // Fetch from background API
  api.RUNTIME("getSettings", null, (response) => {
    let savedTheme =
      response.settings && response.settings.sk_active_theme
        ? response.settings.sk_active_theme
        : "dracula";
    applyTheme(savedTheme, false); // false prevents endless save loop on load
  });
}

// ----------------------------------------------------------
// Omnibar Mapping (`st`) with Visual Indicator
// ----------------------------------------------------------
mapkey("st", "#12Open Theme Switcher (Omnibar)", function () {
  let currentUrl = window.location.href.split("#")[0];
  let themesList = Object.keys(themes).map((key) => {
    let isCurrent = key === activeThemeKey;
    // Visual indicator checkmark if active, palette icon if not
    let icon = isCurrent ? "✅ " : "🎨 ";
    let activeText = isCurrent ? " (Active)" : "";

    return {
      title: `${icon}${themes[key].name}${activeText}`,
      url: currentUrl + "#sk_theme_" + key,
    };
  });

  Front.openOmnibar({ type: "UserURLs", extra: themesList, tabbed: false });
});

// ----------------------------------------------------------
// Cross-Tab Live Sync
// ----------------------------------------------------------
try {
  let storageApi = typeof browser !== "undefined" ? browser : chrome;
  if (storageApi && storageApi.storage && storageApi.storage.onChanged) {
    storageApi.storage.onChanged.addListener(function (changes, namespace) {
      if (
        namespace === "local" &&
        changes.sk_active_theme &&
        changes.sk_active_theme.newValue
      ) {
        let incomingTheme = changes.sk_active_theme.newValue;
        if (incomingTheme !== activeThemeKey) {
          applyTheme(incomingTheme, false);
        }
      }
    });
  }
} catch (e) {} // Failsafe
