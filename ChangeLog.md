# Revision history for MidnightMover

## 0.0.0.0

* Fix parsing of SimsMix after the forum moved from XenForo 1.x to XenForo 2.3:
  messages are now `<article class="message--post">` with the body in
  `<article class="message-body">` (was `<ol id="messageList">` + `<li>`
  + `div.messageContent`).
* Adapt post conversion to the new markup: `blockquote.bbCodeBlock--quote`
  (author from `data-quote`), `div.bbCodeSpoiler`, `s` tag for strikethrough,
  images via `img[data-url]` (proxy.php aware), skip `<script>` and helper divs.
* Fix `[color=...]` closing tag (`[/img]` -> `[/color]`).
* `savePost` now creates `posts/` automatically and sanitizes file names.
* Translate the hardcoded page list to the new pagination (old 10 posts/page,
  new 20 posts/page). Currently set to scan the first 40 pages (`[2 .. 40]`).
  Real last page is read from the XF2 page-jump input (`max` attribute).
* Add `directory` to build dependencies (cabal, default.nix, flake.nix).
* Write DESC.md with a full project description.
* Selenium: `punbbConfig` is now configurable via env (`MM_WD_BASE_PATH` for
  standalone geckodriver vs selenium-server, `MM_HEADLESS=1` for headless).
* Fix `triggerProcessForm`: wrap `executeJS` in `ignoreReturn` (aeson in this
  stack cannot parse a null JS result as `()`).
* Bunker credentials live in `secret/mm.json` (bunker login `tx`).
* Verified end-to-end: scraped 40 pages (68 posts), posted all 68 to
  gamestories topic 22 (`viewtopic.php?id=22`) via Selenium (selenium-server
  3.141.59 + geckodriver + headless Firefox); two posts were rejected by the
  forum and re-posted in a second pass.
* Post rendering fixes (found by comparing the posted copy of the thread
  starter with the original):
  * Tables: new `PostTable` element; XF2 `<table><tr><td>` is converted to
    `[table][tr][td]…[/td][/tr][/table]` (supported by the target forum).
  * Smilies: `img.smilie` now emits `[img]` with the absolute simsmix smiley
    URL instead of the text code (the target forum has a different smiley
    set, so codes like `:sims_sims4:` did not render there).
  * `font-size: Npx` spans: new `PostSize` element -> `[size=N]` (the target
    forum treats `[size=N]` as N pixels).
  * `font-family: X` spans: new `PostFont` element -> `[font=X]`.
* Verified: the re-generated thread-starter post renders with a proper table
  (21 rows), all 46 smilies as images, 18px text size, and ~100% text fidelity
  vs the simsmix original.
* `[align=center]` fix: the target forum's BBCode parser leaves `[align=…]`
  as literal text when it wraps block-level tags ([spoiler]/[table]/…).
  `PostCentered` now splits its content into runs and wraps only the
  non-blocky ones; block content is emitted unwrapped.
* `PostSmilie`: smilies render as `[img]` (so they show on the target forum)
  but are a distinct element so they do not count as "images" in the
  story-post filter (isValidPost) — otherwise posts with only spoilers +
  smilies would pass the filter.
