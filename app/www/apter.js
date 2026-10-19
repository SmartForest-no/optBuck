/* app/www/apter.js
   Shows the run progress bar as soon as Run is clicked, before the server
   answers (Phase 4d); the server's run_status output then replaces it with
   staged progress. Cleared when the server rejects the run.
   Also: class toggles and flat price edits for the Assortments and prices tab. */
(function () {
  var html = '<div class="status-running">Starting run...' +
             '<div class="run-progress"><div class="run-progress-bar is-indeterminate"></div></div></div>';
  $(document).on('click', '#run_btn', function () {
    if ($(this).prop('disabled')) return;
    $('#run_status').html(html);
  });
  $(document).on('shiny:connected', function () {
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
