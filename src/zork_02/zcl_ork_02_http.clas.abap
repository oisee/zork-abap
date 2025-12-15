CLASS zcl_ork_02_http DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PRIVATE SECTION.
    METHODS get_html
      RETURNING VALUE(rv_html) TYPE string.
ENDCLASS.

CLASS zcl_ork_02_http IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA(lv_html) = get_html( ).

    server->response->set_header_field(
      name  = 'Content-Type'
      value = 'text/html; charset=utf-8' ).

    server->response->set_cdata( lv_html ).
  ENDMETHOD.

  METHOD get_html.
    DATA(lv_n) = cl_abap_char_utilities=>newline.

    rv_html =
      |<!DOCTYPE html>{ lv_n }| &&
      |<html>{ lv_n }| &&
      |<head>{ lv_n }| &&
      |  <title>ZORK on SAP HANA</title>{ lv_n }| &&
      |  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css" />{ lv_n }| &&
      |  <style>{ lv_n }| &&
      |    * \{ box-sizing: border-box; \}{ lv_n }| &&
      |    body \{ background-color: #0a0a0a; display: flex; flex-direction: column; justify-content: center; align-items: center; min-height: 100vh; margin: 0; font-family: 'Courier New', monospace; \}{ lv_n }| &&
      |    h1 \{ color: #00ff00; text-shadow: 0 0 10px #00ff00; margin-bottom: 10px; \}{ lv_n }| &&
      |    #terminal-container \{ border: 2px solid #00ff00; border-radius: 8px; padding: 10px; background: #000; box-shadow: 0 0 20px rgba(0, 255, 0, 0.3); overflow: hidden; width: 90vw; max-width: 1200px; \}{ lv_n }| &&
      |    #terminal \{ width: 100%; height: 70vh; \}{ lv_n }| &&
      |    #status \{ color: #888; margin-top: 10px; font-size: 12px; \}{ lv_n }| &&
      |    .connected \{ color: #00ff00 !important; \}{ lv_n }| &&
      |    .disconnected \{ color: #ff4444 !important; \}{ lv_n }| &&
      |    /* Retro scrollbar - WebKit (Chrome/Safari/Edge) */{ lv_n }| &&
      |    ::-webkit-scrollbar \{ width: 12px; height: 12px; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-track \{ background: #0a0a0a; border: 1px solid #003300; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-thumb \{ background: #00aa00; border: 1px solid #00ff00; border-radius: 2px; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-thumb:hover \{ background: #00ff00; box-shadow: 0 0 8px #00ff00; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-corner \{ background: #0a0a0a; \}{ lv_n }| &&
      |    /* Retro scrollbar - Firefox */{ lv_n }| &&
      |    * \{ scrollbar-width: thin; scrollbar-color: #00aa00 #0a0a0a; \}{ lv_n }| &&
      |    /* xterm viewport scrollbar */{ lv_n }| &&
      |    .xterm-viewport::-webkit-scrollbar \{ width: 10px; \}{ lv_n }| &&
      |    .xterm-viewport::-webkit-scrollbar-track \{ background: #000; border-left: 1px solid #003300; \}{ lv_n }| &&
      |    .xterm-viewport::-webkit-scrollbar-thumb \{ background: linear-gradient(180deg, #004400 0%, #00aa00 50%, #004400 100%); border: 1px solid #00ff00; \}{ lv_n }| &&
      |    .xterm-viewport::-webkit-scrollbar-thumb:hover \{ background: #00ff00; box-shadow: 0 0 10px #00ff00, inset 0 0 5px #003300; \}{ lv_n }| &&
      |  </style>{ lv_n }| &&
      |</head>{ lv_n }| &&
      |<body>{ lv_n }| &&
      |  <h1>ZORK on SAP HANA</h1>{ lv_n }| &&
      |  <div id="terminal-container"><div id="terminal"></div></div>{ lv_n }| &&
      |  <div id="status">Status: <span id="statusText" class="disconnected">Connecting...</span></div>{ lv_n }| &&
      |  <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.js"></script>{ lv_n }| &&
      |  <script>{ lv_n }| &&
      |    const APC_PATH = "/sap/bc/apc/sap/zork";{ lv_n }| &&
      |    let socket = null;{ lv_n }| &&
      |    let currentLine = "";{ lv_n }| &&
      |    const term = new Terminal(\{| &&
      |      cursorBlink: true,| &&
      |      fontFamily: '"Courier New", Courier, monospace',| &&
      |      fontSize: 16,| &&
      |      cols: 100,| &&
      |      rows: 30,| &&
      |      theme: \{ background: '#000000', foreground: '#00ff00', cursor: '#00ff00' \}| &&
      |    \});{ lv_n }| &&
      |    term.open(document.getElementById('terminal'));{ lv_n }| &&
      |    function updateStatus(text, connected) \{| &&
      |      document.getElementById('statusText').textContent = text;| &&
      |      document.getElementById('statusText').className = connected ? 'connected' : 'disconnected';| &&
      |    \}{ lv_n }| &&
      |    function connect() \{| &&
      |      const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';| &&
      |      const wsUrl = protocol + '//' + location.host + APC_PATH;| &&
      |      term.writeln('\\x1b[33mConnecting to Z-Machine...\\x1b[0m');| &&
      |      updateStatus('Connecting...', false);| &&
      |      socket = new WebSocket(wsUrl);{ lv_n }| &&
      |      socket.onopen = () => \{| &&
      |        term.writeln('\\x1b[32m[CONNECTED]\\x1b[0m\\r\\n');| &&
      |        updateStatus('Connected - Playing ZORK', true);| &&
      |      \};{ lv_n }| &&
      |      socket.onmessage = (e) => \{| &&
      |        term.write(e.data.replace(/\\r?\\n/g, '\\r\\n'));| &&
      |      \};{ lv_n }| &&
      |      socket.onclose = () => \{| &&
      |        term.writeln('\\r\\n\\x1b[31m[DISCONNECTED]\\x1b[0m');| &&
      |        updateStatus('Disconnected', false);| &&
      |        currentLine = "";| &&
      |      \};{ lv_n }| &&
      |      socket.onerror = () => \{| &&
      |        term.writeln('\\r\\n\\x1b[31m[CONNECTION ERROR]\\x1b[0m');| &&
      |        updateStatus('Error', false);| &&
      |      \};{ lv_n }| &&
      |    \}{ lv_n }| &&
      |    term.onData(data => \{| &&
      |      if (!socket \|\| socket.readyState !== WebSocket.OPEN) return;| &&
      |      const code = data.charCodeAt(0);{ lv_n }| &&
      |      if (code === 13) \{| &&
      |        term.write('\\r\\n');| &&
      |        socket.send(currentLine);| &&
      |        currentLine = "";| &&
      |      \} else if (code === 127 \|\| code === 8) \{| &&
      |        if (currentLine.length > 0) \{| &&
      |          currentLine = currentLine.slice(0, -1);| &&
      |          term.write('\\b \\b');| &&
      |        \}| &&
      |      \} else if (code >= 32) \{| &&
      |        currentLine += data;| &&
      |        term.write(data);| &&
      |      \}| &&
      |    \});{ lv_n }| &&
      |    connect();{ lv_n }| &&
      |  </script>{ lv_n }| &&
      |</body>{ lv_n }| &&
      |</html>|.
  ENDMETHOD.

ENDCLASS.
