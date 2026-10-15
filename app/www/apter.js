/* app/www/apter.js
   Shows the run progress bar as soon as Run is clicked, before the server
   answers (Phase 4d); the server's run_status output then replaces it with
   staged progress. Cleared when the server rejects the run. */
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
  });
})();
