# MidnightMover — описание проекта

Проект переносит посты из форума **SimsMix** (`https://simsmix.ru/forum`, XenForo) в
форум **BackyardStuff** (`https://gamestories.clanboard.ru`, PunBB). Работает в двух
режимах, которые выбираются полем `output` в `user.txt`:

1. **OutputFile** — читает посты с SimsMix, конвертирует их BBCode в свой
   внутренний формат и сохраняет каждый пост отдельным текстовым файлом
   (чистый BBCode) в `posts/`. Список сохранённых файлов пишется в `desc.json`.
2. **OutputBunker** — берёт список файлов из `desc.json`, читает их и постит
   содержимое на gamestories.clanboard.ru через Selenium (WebDriver),
   обходя антибот-защиту (скрипт `process_form`, токены `formkey`/`formetc`).

Чтение с SimsMix делается обычным HTTP (wreq / http-client + cookies), запись в
бункер — через Selenium (пакет `webdriver`, Firefox с отключённым JS).

---

## Структура репозитория

| Файл | Назначение |
|---|---|
| `src/Main.hs` | Точка входа: читает `user.txt`, выбирает режим по `output`, запускает `getMessages` (режим файла) или `copyToBunker` (режим бункера). |
| `src/Ctx.hs` | Состояние (`Ctx`: cookie jar, base URL, TLS manager, пользователь), типы `User`/`Output`/`ErrorKind`, логин в SimsMix (XenForo CSRF-токен `_xfToken`), загрузка страниц (`getPageCursor`), обработка страниц и сообщений (`getMessages`/`getPageMessages`), хардкод-список страниц. |
| `src/Parse.hs` | Разбор HTML страницы темы: поиск сообщений (`article.message--post`) и их тел (`article.message-body`). |
| `src/Post.hs` | Конвертация HTML-тела сообщения (XML Cursor) во внутренние элементы (`PostElement`) и обратно в BBCode (`toBBCMap`); валидация «пост это история или комментарий» (`isValidPost`); сохранение в файл (`savePost`); номер последней страницы (`exractPageNumber`). |
| `src/Bunker.hs` | Selenium-сессия: логин в PunBB, извлечение токенов `process_form`, отправка сообщений в тему (сейчас хардкод `tid=22`). |
| `src/Antibot.hs` | Находит inline-скрипт `process_form` в `<head>` и выполняет его, чтобы сгенерировать антибот-токены. |
| `src/TlsManager.hs` | Создание TLS `Manager` (отключена проверка сертификатов; `NoEMS`). |
| `user.txt` | JSON: логин/пароль SimsMix, список тем (`threads`), фильтр авторов (`userFilter`), режим (`output`). **В git не коммитится.** |
| `secret/mm.json` | Учётные данные бункера (gamestories): JSON для поля `output` (`["tx", "<пароль>"]`) и подсказка про selenium-server. **В git не коммитится.** |
| `desc.json` | Список сохранённых файлов постов (пути вида `./posts/post-<id>-<автор>.txt`). |
| `posts/` | Каталог с сохранёнными постами (создаётся автоматически; файлы `post-*.txt` игнорируются git). |
| `response` | Дамп тела ответа логина SimsMix (перезаписывается при каждом запуске; закоммичен как отладочный артефакт). |
| `punbb_login.sh`, `test.js`, `*.html` | Отладочные артефакты прежних подходов (см. ветки git `conduit`/`selenium`/`selenium-save`). |

## Сборка и запуск

**На NixOS** (flake):

```sh
nix build .#  # или nix run .
```

**На Debian/Ubuntu с cabal** (проверено: GHC 8.8.4, cabal 3.16):

```sh
cabal build          # зависимости тянутся из локального store
cabal run MidnightMover
```

> Примечание: в этой песочнице `~/.cabal` доступен только на чтение, поэтому
> используется локальный cabal-каталог `.cabal-local/` с симлинками на индекс
> Hackage и store из `~/.cabal` (см. `.cabal-local/config`). Запуск — с флагом
> `--offline`:
> `CABAL_DIR=$(pwd)/.cabal-local cabal run --offline MidnightMover`.

Зависимости перечислены в `MidnightMover.cabal`; для Nix — в `default.nix` /
`flake.nix` (обязательно держать списки синхронными; добавляли `directory`).

---

## `user.txt`

```json
{
  "login": "apelsinka",
  "password": "0011aa",
  "threads": ["https://simsmix.ru/forum/threads/interaktivnyj-proekt-randomnyj-njukrest.4205/"],
  "userFilter": ["29715000", "Muddy", "orangepink"],
  "output": {"tag": "OutputFile", "contents": null}
}
```

* `threads` — список URL тем; код берёт только `threads !! 0`.
* `userFilter` — авторы, посты которых переносятся (остальные пропускаются).
* `output`:
  * `{"tag": "OutputFile", "contents": null}` — режим 1 (сохранить в `posts/`).
  * `{"tag": "OutputBunker", "contents": [login, password]}` — режим 2 (постить в бункер).

Внимание: логин/пароль в `user.txt` — **секрет**, файл в `.gitignore`.

---

## Режим 1: SimsMix → локальные файлы

Поток: `Main` → `Ctx.getMessages` → для каждой страницы `getPageMessages` →
`getPageMessage` → `Post.savePost` → список имён файлов → `desc.json`.

### Логин (SimsMix, XenForo 2.3)

1. `GET https://simsmix.ru/forum/login/login` — забрать `_xfToken`
   (`input[name="_xfToken"][value]`) и cookies.
2. `POST` туда же с полями `login`, `register=0`, `password`, `cookie_check=1`,
   `_xfToken`, `redirect=/forum/`. Успех — HTTP 2xx, в теле `data-logged-in="true"`.
3. Cookies (`xf_session`, `xf_csrf`) сохраняются в `Ctx`.

Логин **обязателен**: часть содержимого темы недоступна анонимам.

### Загрузка страниц

`getPageCursor addr` — GET с cookie jar, обновление jar через
`updateCookieJar`, парсинг HTML в XML-дерево (`Text.HTML.DOM.parseLBS`) и
`Text.XML.Cursor.fromDocument`. http-client следует редиректам сам
(канонический URL темы отличается от указанного в `user.txt`).

### Структура сообщений (XenForo 2.x — текущая)

Форум переехал с XF1 на XF 2.3, разметка сообщений изменилась:

```
<article class="message ... message--post js-post js-inlineModContainer"
         data-author="29715000" data-content="post-282681" id="js-post-282681">
  <span class="u-anchorTarget" id="post-282681"></span>
  <div class="message-inner">
    <div class="message-cell message-cell--user">…аватар/ник…</div>
    <div class="message-cell message-cell--main">
      <div class="message-main uix_messageContent js-quickEditTarget">
        <header class="message-attribution …">…</header>
        <div class="message-content js-messageContent">
          <div class="message-userContent lbContainer js-lbContainer">
            <article class="message-body js-selectToQuote">
              <div itemprop="text"><div class="bbWrapper">…BBCode-контент…</div></div>
            </article>
          </div>
          <div class="reactionsBar …">…реакции…</div>
        </div>
      </div>
    </div>
  </div>
  <aside class="message-signature">…подпись (вне message-body!)…</aside>
</article>
```

Старая (XF1) разметка, на которую был заточен парсер: `<ol id="messageList">`
+ `<li id="post-…" data-author="…">` + `<div class="messageContent">`.

В `Parse.hs`:
* `extractMessages` — ищет все `article` с классом `message--post`
  (раньше: `ol#messageList > li`).
* `msgIdOf` — id поста из `data-content` (`post-282681`); запасной вариант —
  `id="js-post-…"` с обрезанием префикса `js-post-`.
* `isMessage` — требует `data-author`; телом считается первый
  `article.message-body` внутри (раньше: `div.messageContent`).

### Преобразование HTML → BBCode (`Post.hs`)

Тело поста (`article.message-body`) разбирается рекурсивно (`extractPost`).
Соответствие элементов XF2:

| HTML (XF2) | Элемент | BBCode |
|---|---|---|
| `img` с `data-url`/`src` (http) | `PostImageGlobal` | `[img]url[/img]` |
| `img` с относительным `src` (смайлик/локальный) | `PostImageLocal alt` | текст `alt` |
| `blockquote.bbCodeBlock--quote` (автор в `data-quote`) | `PostQuote` | `[quote=автор]…[/quote]` |
| `div.bbCodeSpoiler` (заголовок в `span.bbCodeSpoiler-button-title`, тело в `div.bbCodeSpoiler-content`) | `PostSpoiler` | `[spoiler=заголовок]…[/spoiler]` |
| `div.dice_outer` + `span.dice_number` | `PostDice` | `текст: значение` |
| `div`/`p` со `style="text-align: center"` | `PostCentered` | `[align=center]…[/align]` |
| `span` со `style="color: …"` | `PostColor` | `[color=…]…[/color]` |
| `span` со `style="font-size: Npx"` | `PostSize` | `[size=N]` (бункер трактует `[size=N]` как N px) |
| `span` со `style="font-family: X"` | `PostFont` | `[font=X]` |
| `table` / `tr` / `td` (XF2, класс `brtb_item_table`) | `PostTable` | `[table][tr][td]…[/td][/tr][/table]` |
| `img.smilie` | `PostSmilie` | `[img]абсолютный URL смайла[/img]` (в фильтре `isValidPost` картинкой не считается) |
| `b`, `i`, `u`, `s` | `PostFormated` | `[b]…[/b]` и т.п. |
| `a[href]` | `PostLink` | `[url=href]текст[/url]` |
| `iframe` | `PostYouTube` | `[video]src[/video]` |
| `br` | `PostLineBreak` | `\n` |
| `script` | — | пропускается |
| `div.js-selectToQuoteEnd`, `div.bbCodeBlock-expandLink` | — | пропускается |

Особенности текущей разметки, учтённые в коде:
* Картинки обёрнуты в `div.bbImageWrapper`; реальный URL берётся из
  `img[data-url]` (у proxy-ссылок `src="/forum/proxy.php?image=…"` относительный).
* Смайлы (`img.smilie`) превращаются в `[img]` с абсолютным URL — у бункера
  свой набор смайлов, и текстовые коды вроде `:sims_sims4:` там не рендерятся.
* Таблицы (`<table class="brtb_item_table">` — в посте 282681 «пантеон
  демиургов») конвертируются в `[table][tr][td]…[/td][/tr][/table]`; целевой
  форум этот тег понимает (проверено пробным постом).
* Внутри цитат контент лежит в `div.bbCodeBlock-expandContent`
  (скрипт-патч lightbox и ссылка «Нажмите для раскрытия…» отбрасываются).
* Внутри постов встречаются `<script>` (JSON фраз / патчи) — отбрасываются.
* Пустые (whitespace-only) текстовые узлы не порождают строк; итоговый файл
  обрезается по краям.

Проверка рендеринга на бункере (пробный пост): `[table]`, `[tr]`, `[td]`,
`[s]`, `[font=X]`, `[hr]`, `[video]`, `[color=…]`, `[size=N]` (N — px)
поддерживаются; `[size=18px]`, `[list]` и `[align=…]` вокруг блочных тегов
(`[spoiler]`, `[table]`, …) — НЕ поддерживаются (выводятся как текст).
Поэтому: `font-size: Npx` → `[size=N]`; списки остаются плоскими текстом;
`PostCentered` оборачивает в `[align=center]` только «не-блочные» фрагменты
(текст, картинки, смайлы), а спойлеры/таблицы/цитаты/кубики оставляет как есть.

### Фильтр «это пост истории или комментарий» (`isValidPost`)

Для каждого поста считается тройка флагов (спойлер, картинка, кубик):
* `PostDice` → `bd=True`
* `PostSpoiler` → `bs=True`
* `PostImageGlobal` → `bi=True`

Пост сохраняется, если `bd || (bs && bi)` — т.е. есть кубик, либо спойлер
с картинкой. Обычные текстовые комментарии отбрасываются (печатается
«пропускаем, это коментарий»).

### Пагинация (важно!)

Старый форум показывал **10 постов на страницу**, новый (XF2) — **20**.
Содержимое темы то же самое (≈5463 поста, последняя страница 274).
Старые номера страниц, зашитые в код, переведены в новые делением пополам:
141→71, 502→251, 503→252, 505→253, 507→254, 518→259, 522→261, 532→266.
Сейчас (задача «скан первых 40 страниц») список в `Ctx.hs` выставлен как
`[2 .. 40]` — т.е. страницы 1..40 темы.

Список живёт в `Ctx.hs` (`getMessages` и `move`), рядом комментарий.
Тема `threads !! 0` всегда обрабатывается первой (страница 1). Номер последней
страницы берётся из XF2-пагинатора (`input.js-pageJumpPage[max]`, см.
`Post.exractPageNumber`; раньше был `div.PageNav[data-last]`) и печатается как
«Total: …».

Примечание: XF2 показывает стартовый пост темы (id 282681) на **каждой**
странице как «закреплённый» — дубликаты схлопываются `nub` в `getMessages`.

### Выходные файлы

* `posts/post-<id>-<автор>.txt` — чистый BBCode поста (именно это содержимое
  потом постится в бункер; никаких заголовков/метаданных в файл не добавлять!).
* `desc.json` — список этих файлов (пути `./posts/…`).

---

## Режим 2: загрузка в бункер (Selenium)

`Main` (при `output = OutputBunker login password`) читает `desc.json`, читает
все файлы и вызывает `Bunker.copyToBunker login password msgs`.
**Учётные данные бункера** (логин `tx`) лежат в `secret/mm.json`
(формат: строка JSON для поля `output` + строка-подсказка
`selenium-server --port 4444`).

Антибот-схема PunBB (`gamestories.clanboard.ru`):
1. `loginPunBB` — открыть `login.php`, заполнить `fld1`/`fld2`, нажать `login`.
   (форма уходит на `login.php?action=in`; браузер делает это сам по action формы)
2. Открыть страницу ответа `post.php?tid=22` (id темы захардкожен; целевая тема —
   `viewtopic.php?id=22`, «Ньюкрест (архив)», там уже есть пост-анонс от 29715000).
3. `Antibot.loadOnlyProcessFormScript` — найти в `<head>` inline-скрипт с
   `process_form` и выполнить его (JS в браузере Firefox при этом отключён —
   `javascript.enabled = False` в капабилити).
4. `triggerProcessForm` — вызвать `process_form(form)` для заполнения скрытых
   полей в `div#formkey` и `div#formetc`.
5. `postMessage` — вписать текст в `textarea[name=req_message]`, ещё раз
   вызвать `process_form`, нажать `submit`. Первое сообщение уходит через
   полный редактор `post.php?tid=22`, последующие — через быстрый ответ
   на странице темы (там тот же `form id="post"`, `req_message`, `formkey`/`formetc`
   и `process_form` в `<head>`; `openPage` внутри `postMessage` закомментирован
   намеренно).

Для работы нужен WebDriver на `localhost:4444`. Настройки читаются из окружения
(см. `Bunker.punbbConfig`):
* `MM_WD_BASE_PATH` — базовый путь WebDriver. По умолчанию `/wd/hub`
  (selenium-server). Для standalone geckodriver задать пустым: `MM_WD_BASE_PATH=""`.
* `MM_HEADLESS=1` — добавить `-headless` в `moz:firefoxOptions` (для окружений
  без дисплея).

### Проверенная локальная связка (Debian-песочница)

Библиотека `webdriver-0.12.0.1` говорит по **legacy JSON Wire протоколу**
(`POST /session` с `desiredCapabilities`), а современный geckodriver (≥0.30)
принимает только W3C (`capabilities`). Поэтому связка
**webdriver → selenium-server** (переводчик) работает, а напрямую к
geckodriver — нет. Проверено:

* `selenium-server-standalone-3.141.59.jar` (Java 17, Temurin JRE) + `geckodriver`
  на PATH + `-Dwebdriver.firefox.bin=…` + Firefox (headless) → legacy `/wd/hub/session`
  отвечает, всё работает. Selenium 4.x legacy НЕ поддерживает
  (`HTTP method not allowed` на `/wd/hub/session`).
* Запуск: `java -Dwebdriver.gecko.driver=…/geckodriver
  -Dwebdriver.firefox.bin=…/firefox/firefox -jar selenium-server-standalone-3.jar
  -port 4444` (geckodriver и firefox — в `tools/`, каталог в `.gitignore`).

### Грабли: `executeJS` и aeson

`executeJS … :: WD ()` падает с `BadJSON "parsing () failed, expected Array,
but encountered Null"`, когда JS возвращает `null` (в этой версии aeson
`()` разбирается только из массива). Поэтому вызов `process_form` обёрнут в
`ignoreReturn $ executeJS …` (см. `Bunker.triggerProcessForm`) — как и советует
документация библиотеки.

### Проверка после заливки

После прогона стоит сверить, что все файлы из `desc.json` реально уехали на
форум: сравнить текст постов темы с локальными файлами (с учётом того, что
форум рендерит BBCode в HTML). В тестовом прогоне 68 постов два не ушли
(сервер их отклонил), их дозалили вторым прогоном с временным `desc.json`
только с этими двумя файлами.

---

## Известные особенности / грабли

* `savePost` создаёт каталог `posts/` сам (`createDirectoryIfMissing`).
  Раньше его надо было создавать вручную.
* Имена файлов санитизируются (`/\:*?"<>|` и управляющие символы → `_`).
* `toBBC (PostColor …)` раньше закрывал тег `[/img]` вместо `[/color]` — исправлено.
* Старые классы XF1 (`messageContent`, `bbCodeQuote`, `bbCodeSpoilerContainer`,
  `SpoilerTitle`, `bbCodeSpoilerText`, `PageNav`) оставлены как fallback в коде,
  но в текущей разметке не встречаются.
* В `Ctx.hs` печатается cookie jar при каждой загрузке страницы
  (`liftIO $ print (cookieJar req)`) — шумно, но безвредно.
* Страницы 502+ из старого списка сейчас редиректят на последнюю страницу (274) —
  поэтому список переведён на новые номера, а сейчас выставлен на `[2 .. 40]`
  (скан первых 40 страниц).
* `stack.yaml`/`stack.yaml.lock` в репозитории — от локальной сборки через
  Stack на NixOS; основной путь сборки — cabal/flake.
* Для сборки в песочнице с read-only `~/.cabal` используется
  `CABAL_DIR=.cabal-local` + `--offline` (симлинки на индекс и store).
* Скачанные браузеры/драйверы (Firefox, geckodriver, JRE, selenium jar) лежат в
  `tools/` (в `.gitignore`), чтобы можно было перезапустить заливку в песочнице.
* `secret/mm.json` — учётные данные бункера (поле `output` для `user.txt`);
  каталог `secret/` в `.gitignore`.
