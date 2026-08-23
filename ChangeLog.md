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
  new 20 posts/page: 141->71, 502->251, ...). Real last page is read from the
  XF2 page-jump input (`max` attribute).
* Add `directory` to build dependencies (cabal, default.nix, flake.nix).
* Write DESC.md with a full project description.
