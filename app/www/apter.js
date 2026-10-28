/* app/www/apter.js
   Shows the run progress bar as soon as Run is clicked, before the server
   answers (Phase 4d); the server's run_status output then replaces it with
   staged progress. Cleared when the server rejects the run.
   Also: class toggles and flat price edits for the Assortments and prices tab.
   Language (Phase 1, 2026-10-27): the navbar switch (Norsk | English). The
   choice comes from ?lang= in the URL, else the browser's last choice
   (localStorage), else the page's start language (meta apter-lang). Static
   texts carry data-i18n (their id in the translation table) and are swapped
   here; the server re-renders its own outputs from input$lang. */
(function () {
  var dict = null, lang = null;
  function texts() {
    if (!dict) {
      var el = document.getElementById('apter-i18n');
      try { dict = el ? JSON.parse(el.textContent) : {}; } catch (e) { dict = {}; }
    }
    return dict;
  }
  function t(id) {
    var d = texts()[lang] || {};
    return d[id] !== undefined ? d[id] : null;
  }
  // sprintf-like: %s, %d, %g, %.1f in order (arguments are text already).
  function fill(s, args) {
    var i = 0;
    return s.replace(/%%|%(?:\d+\$)?[-+ 0#]*\d*(?:\.\d+)?[sdifg]/g, function (m) {
      if (m === '%%') return '%';
      var pos = m.match(/^%(\d+)\$/);
      return pos ? args[parseInt(pos[1], 10) - 1] : args[i++];
    });
  }
  function startLang() {
    var q = new URLSearchParams(window.location.search).get('lang');
    if (q === 'no' || q === 'en') return q;
    try {
      var s = window.localStorage.getItem('apter.lang');
      if (s === 'no' || s === 'en') return s;
    } catch (e) { /* no storage: the page's language */ }
    var m = document.querySelector('meta[name="apter-lang"]');
    return m ? m.getAttribute('content') : 'no';
  }
  function apply() {
    document.querySelectorAll('[data-i18n]').forEach(function (el) {
      var s = t(el.getAttribute('data-i18n'));
      if (s === null) return;
      var a = el.getAttribute('data-i18n-args');
      el.textContent = a ? fill(s, JSON.parse(a)) : s;
    });
    document.querySelectorAll('[data-i18n-ph]').forEach(function (el) {
      var s = t(el.getAttribute('data-i18n-ph'));
      if (s !== null) el.setAttribute('placeholder', s);
    });
    uploadBars();
    // The altitude slider's labels: "1 100 m" in Norsk, "1,100 m" in English.
    var sl = window.jQuery && jQuery('#hoh_range').data('ionRangeSlider');
    if (sl && sl.options.prettify_separator !== (lang === 'no' ? ' ' : ','))
      sl.update({prettify_separator: lang === 'no' ? ' ' : ','});
    var title = t('window_title');
    if (title !== null) document.title = fill(title, ['optBuck']);
    document.documentElement.setAttribute('lang', lang === 'no' ? 'nb' : 'en');
    document.querySelectorAll('.lang-btn').forEach(function (b) {
      var on = b.getAttribute('data-lang') === lang;
      b.classList.toggle('is-active', on);
      b.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }
  // Shiny writes "Upload complete" into a file input's bar itself: shown in
  // the viewer's language (on each upload, and on a switch).
  function uploadBars() {
    var done = [texts().en ? texts().en.file_done : null, texts().no ? texts().no.file_done : null];
    document.querySelectorAll('.shiny-file-input-progress .progress-bar').forEach(function (b) {
      if (done.indexOf(b.textContent) >= 0 && t('file_done') !== null && b.textContent !== t('file_done'))
        b.textContent = t('file_done');
    });
  }
  new MutationObserver(function () { if (lang) uploadBars(); })
    .observe(document.documentElement, {childList: true, subtree: true, characterData: true});
  function setLang(l, remember) {
    lang = l;
    if (remember) { try { window.localStorage.setItem('apter.lang', l); } catch (e) { } }
    apply();
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('lang', l);
  }
  document.addEventListener('DOMContentLoaded', function () { setLang(startLang(), false); });
  $(document).on('click', '.lang-btn', function () { setLang($(this).data('lang'), true); });

  $(document).on('click', '#run_btn', function () {
    if ($(this).prop('disabled')) return;
    var html = '<div class="status-running">' + (t('run_starting') || 'Starting run...') +
               '<div class="run-progress"><div class="run-progress-bar is-indeterminate"></div></div></div>';
    $('#run_status').html(html);
  });
  // Inputs bound by Shiny (the slider) exist only now.
  $(document).on('shiny:sessioninitialized', function () { if (lang) apply(); });
  $(document).on('shiny:connected', function () {
    Shiny.setInputValue('lang', lang || startLang());
    Shiny.addCustomMessageHandler('apter_progress_clear', function (msg) {
      $('#run_status').empty();
    });
    // Highlight a block (e.g. a flat price that differs from the other scenario).
    Shiny.addCustomMessageHandler('apter_toggle_class', function (msg) {
      $('#' + msg.id).toggleClass(msg.cls, !!msg.on);
    });
  });
  // A flat price edit carries its scenario and assortment key, so a late
  // edit is never written to another assortment (server_scenarios.R).
  $(document).on('change', '.asm-flat-input input', function () {
    var w = $(this).closest('.asm-flat-input');
    Shiny.setInputValue('flat_edit', {side: w.data('side'), key: w.data('key'), value: this.value},
                        {priority: 'event'});
  });
})();
