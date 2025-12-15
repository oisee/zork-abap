CLASS zcl_bbs_http_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PRIVATE SECTION.
    METHODS get_html
      RETURNING VALUE(rv_html) TYPE string.
ENDCLASS.

CLASS zcl_bbs_http_handler IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA(lv_html) = get_html( ).

    server->response->set_header_field(
      name  = 'Content-Type'
      value = 'text/html; charset=utf-8' ).

    server->response->set_cdata( lv_html ).
  ENDMETHOD.

  METHOD get_html.
    DATA(lv_crlf) = cl_abap_char_utilities=>cr_lf.

    rv_html =
      |<!DOCTYPE html>{ lv_crlf }| &&
      |<html>{ lv_crlf }| &&
      |<head>{ lv_crlf }| &&
      |  <title>SAP HANA BBS Terminal</title>{ lv_crlf }| &&
      |  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css" />{ lv_crlf }| &&
      |  <style>{ lv_crlf }| &&
      |    * \{ box-sizing: border-box; \}{ lv_crlf }| &&
      |    body \{ background-color: #0a0a0a; display: flex; flex-direction: column; justify-content: center; align-items: center; min-height: 100vh; margin: 0; font-family: 'Courier New', monospace; \}{ lv_crlf }| &&
      |    h1 \{ color: #00ff00; text-shadow: 0 0 10px #00ff00; margin-bottom: 10px; \}{ lv_crlf }| &&
      |    #terminal-container \{ border: 2px solid #00ff00; border-radius: 8px; padding: 10px; background: #000; box-shadow: 0 0 20px rgba(0, 255, 0, 0.3); \}{ lv_crlf }| &&
      |    #terminal \{ width: 820px; height: 500px; \}{ lv_crlf }| &&
      |    #status \{ color: #888; margin-top: 10px; font-size: 12px; \}{ lv_crlf }| &&
      |    .connected \{ color: #00ff00 !important; \}{ lv_crlf }| &&
      |    .disconnected \{ color: #ff4444 !important; \}{ lv_crlf }| &&
      |  </style>{ lv_crlf }| &&
      |</head>{ lv_crlf }| &&
      |<body>{ lv_crlf }| &&
      |  <h1>SAP HANA BBS Terminal</h1>{ lv_crlf }| &&
      |  <div id="terminal-container"><div id="terminal"></div></div>{ lv_crlf }| &&
      |  <div id="status">Status: <span id="statusText" class="disconnected">Disconnected</span></div>{ lv_crlf }| &&
      |  <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.js"></script>{ lv_crlf }| &&
      |  <script>{ lv_crlf }| &&
      |    const APC_PATH = "/sap/bc/apc/sap/zapc_bbs_echo";{ lv_crlf }| &&
      |    let socket = null;{ lv_crlf }| &&
      |    let currentLine = "";{ lv_crlf }| &&
      |    const term = new Terminal(\{ cursorBlink: true, fontFamily: '"Courier New", Courier, monospace', fontSize: 16, cols: 100, rows: 25, theme: \{ background: '#000000', foreground: '#00ff00', cursor: '#00ff00' \} \});{ lv_crlf }| &&
      |    term.open(document.getElementById('terminal'));{ lv_crlf }| &&
      |    function updateStatus(text, connected) \{ document.getElementById('statusText').textContent = text; document.getElementById('statusText').className = connected ? 'connected' : 'disconnected'; \}{ lv_crlf }| &&
      |    function connect() \{| &&
      |      const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';{ lv_crlf }| &&
      |      const wsUrl = protocol + '//' + location.host + APC_PATH;{ lv_crlf }| &&
      |      term.writeln('\\x1b[33mConnecting to ' + wsUrl + '...\\x1b[0m');{ lv_crlf }| &&
      |      updateStatus('Connecting...', false);{ lv_crlf }| &&
      |      socket = new WebSocket(wsUrl);{ lv_crlf }| &&
      |      socket.onopen = () => \{ term.writeln('\\x1b[32m[CONNECTED]\\x1b[0m\\r\\n'); updateStatus('Connected', true); \};{ lv_crlf }| &&
      |      socket.onmessage = (e) => \{ term.write(e.data.replace(/\\r?\\n/g, '\\r\\n')); \};{ lv_crlf }| &&
      |      socket.onclose = () => \{ term.writeln('\\r\\n\\x1b[31m[DISCONNECTED]\\x1b[0m'); updateStatus('Disconnected', false); \};{ lv_crlf }| &&
      |      socket.onerror = () => \{ term.writeln('\\r\\n\\x1b[31m[ERROR]\\x1b[0m'); updateStatus('Error', false); \};{ lv_crlf }| &&
      |    \}{ lv_crlf }| &&
      |    term.onData(data => \{| &&
      |      if (!socket \|\| socket.readyState !== WebSocket.OPEN) return;{ lv_crlf }| &&
      |      const code = data.charCodeAt(0);{ lv_crlf }| &&
      |      if (code === 13) \{ term.write('\\r\\n'); socket.send(currentLine); currentLine = ""; \}{ lv_crlf }| &&
      |      else if (code === 127 \|\| code === 8) \{ if (currentLine.length > 0) \{ currentLine = currentLine.slice(0, -1); term.write('\\b \\b'); \} \}{ lv_crlf }| &&
      |      else if (code >= 32) \{ currentLine += data; term.write(data); \}{ lv_crlf }| &&
      |    \});{ lv_crlf }| &&
      |    connect();{ lv_crlf }| &&
      |  </script>{ lv_crlf }| &&
      |</body>{ lv_crlf }| &&
      |</html>|.
  ENDMETHOD.

ENDCLASS.