CLASS zcl_ork_02_http DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PRIVATE SECTION.
    METHODS get_html
      IMPORTING iv_env      TYPE string
                iv_sanitize TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_html) TYPE string.
ENDCLASS.

CLASS zcl_ork_02_http IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA(lv_env) = server->request->get_form_field( 'env' ).
    IF lv_env IS INITIAL.
      lv_env = 'DEFAULT.ENV'.
    ENDIF.

    DATA(lv_sanitize) = server->request->get_form_field( 'sanitize' ).
    DATA(lv_sanitize_flag) = xsdbool( lv_sanitize = '1' OR lv_sanitize = 'true' ).

    DATA(lv_html) = get_html( iv_env = lv_env iv_sanitize = lv_sanitize_flag ).

    server->response->set_header_field(
      name  = 'Content-Type'
      value = 'text/html; charset=utf-8' ).

    server->response->set_cdata( lv_html ).
  ENDMETHOD.

  METHOD get_html.
    DATA(lv_n) = cl_abap_char_utilities=>newline.
    DATA(lv_or) = `||`.
    DATA(lv_sanitize_js) = COND string( WHEN iv_sanitize = abap_true THEN 'true' ELSE 'false' ).

    rv_html =
      |<!DOCTYPE html>{ lv_n }| &&
      |<html>{ lv_n }| &&
      |<head>{ lv_n }| &&
      |  <meta charset="UTF-8">{ lv_n }| &&
      |  <title>ZORK on SAP HANA</title>{ lv_n }| &&
      |  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.css" />{ lv_n }| &&
      |  <style>{ lv_n }| &&
      |    * \{ box-sizing: border-box; \}{ lv_n }| &&
      |    body \{ background-color: #0a0a0a; display: flex; flex-direction: column; justify-content: center; align-items: center; min-height: 100vh; margin: 0; font-family: 'Courier New', monospace; \}{ lv_n }| &&
      |    h1 \{ color: #00ff00; text-shadow: 0 0 10px #00ff00; margin-bottom: 10px; \}{ lv_n }| &&
      |    #terminal-container \{ border: 2px solid #00ff00; border-radius: 8px; padding: 10px; background: #000; box-shadow: 0 0 20px rgba(0, 255, 0, 0.3); overflow: hidden; width: 90vw; max-width: 1200px; \}{ lv_n }| &&
      |    #terminal \{ width: 100%; height: 60vh; \}{ lv_n }| &&
      |    #toolbar \{ margin: 15px 0; display: flex; gap: 8px; justify-content: flex-start; align-items: center; min-height: 44px; padding-left: 10px; \}{ lv_n }| &&
      |    .toolbar-group \{ display: flex; gap: 6px; align-items: center; \}{ lv_n }| &&
      |    .btn \{ padding: 10px 16px; font-family: 'Courier New', monospace; font-size: 14px; cursor: pointer; border: 2px solid; border-radius: 4px; transition: all 0.2s; white-space: nowrap; \}{ lv_n }| &&
      |    .btn-play \{ background: #004400; color: #00ff00; border-color: #00ff00; min-width: 70px; \}{ lv_n }| &&
      |    .btn-play:hover \{ background: #006600; box-shadow: 0 0 10px #00ff00; \}{ lv_n }| &&
      |    .btn-play.active \{ background: #00aa00; color: #000; \}{ lv_n }| &&
      |    .btn-watch \{ background: #444400; color: #ffff00; border-color: #ffff00; min-width: 90px; \}{ lv_n }| &&
      |    .btn-watch:hover \{ background: #666600; box-shadow: 0 0 10px #ffff00; \}{ lv_n }| &&
      |    .btn-watch.active \{ background: #aaaa00; color: #000; \}{ lv_n }| &&
      |    .btn-step \{ background: #003344; color: #00ddff; border-color: #00ddff; min-width: 50px; padding: 8px 12px; font-size: 13px; \}{ lv_n }| &&
      |    .btn-step:hover \{ background: #004466; box-shadow: 0 0 10px #00ddff; \}{ lv_n }| &&
      |    .btn-step:disabled \{ opacity: 0.3; cursor: not-allowed; \}{ lv_n }| &&
      |    .btn-run \{ background: #003300; color: #00ff88; border-color: #00ff88; min-width: 60px; \}{ lv_n }| &&
      |    .btn-run:hover \{ background: #004400; box-shadow: 0 0 10px #00ff88; \}{ lv_n }| &&
      |    .btn-run.active \{ background: #00aa44; color: #000; animation: pulse 1s infinite; \}{ lv_n }| &&
      |    .btn-stop \{ background: #440000; color: #ff4444; border-color: #ff4444; min-width: 60px; \}{ lv_n }| &&
      |    .btn-stop:hover \{ background: #660000; box-shadow: 0 0 10px #ff4444; \}{ lv_n }| &&
      |    .btn-export \{ background: #333344; color: #aaaaff; border-color: #aaaaff; padding: 8px 12px; font-size: 12px; \}{ lv_n }| &&
      |    .btn-export:hover \{ background: #444466; box-shadow: 0 0 10px #aaaaff; \}{ lv_n }| &&
      |    @keyframes pulse \{ 0%, 100% \{ opacity: 1; \} 50% \{ opacity: 0.7; \} \}{ lv_n }| &&
      |    .separator \{ color: #333; margin: 0 8px; font-size: 18px; \}{ lv_n }| &&
      |    #turnInfo \{ color: #00ddff; font-size: 13px; width: 280px; text-align: left; padding-left: 10px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; \}{ lv_n }|
      &&
      |    .timeout-control \{ display: flex; align-items: center; gap: 6px; \}{ lv_n }|
      &&
      |    .timeout-control label \{ color: #666; font-size: 11px; \}{ lv_n }|
      &&
      |    .timeout-control input[type="range"] \{ width: 80px; accent-color: #ff8800; \}{ lv_n }|
      &&
      |    .timeout-control span \{ color: #ff8800; font-size: 11px; min-width: 40px; \}{ lv_n }| &&
      |    #envInfo \{ color: #666; font-size: 11px; \}{ lv_n }| &&
      |    #moveCount \{ color: #888; font-size: 11px; margin-left: 10px; \}{ lv_n }| &&
      |    #status \{ color: #888; margin-top: 10px; font-size: 12px; \}{ lv_n }| &&
      |    .connected \{ color: #00ff00 !important; \}{ lv_n }| &&
      |    .disconnected \{ color: #ff4444 !important; \}{ lv_n }| &&
      |    .watching \{ color: #ffff00 !important; \}{ lv_n }| &&
      |    .running \{ color: #00ff88 !important; \}{ lv_n }| &&
      |    ::-webkit-scrollbar \{ width: 12px; height: 12px; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-track \{ background: #0a0a0a; border: 1px solid #003300; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-thumb \{ background: #00aa00; border: 1px solid #00ff00; border-radius: 2px; \}{ lv_n }| &&
      |    ::-webkit-scrollbar-thumb:hover \{ background: #00ff00; box-shadow: 0 0 8px #00ff00; \}{ lv_n }| &&
      |    * \{ scrollbar-width: thin; scrollbar-color: #00aa00 #0a0a0a; \}{ lv_n }| &&
      |  </style>{ lv_n }| &&
      |</head>{ lv_n }| &&
      |<body>{ lv_n }| &&
      |  <h1>ZORK on SAP HANA</h1>{ lv_n }| &&
      |  <div id="toolbar">{ lv_n }| &&
      |    <div class="toolbar-group">{ lv_n }| &&
      |      <button id="btnPlay" class="btn btn-play active" onclick="setMode('play')">Play</button>{ lv_n }| &&
      |      <button id="btnWatch" class="btn btn-watch" onclick="setMode('watch')">Watch AI</button>{ lv_n }| &&
      |    </div>{ lv_n }| &&
      |    <span class="separator">\|</span>{ lv_n }| &&
      |    <div class="toolbar-group">{ lv_n }| &&
      |      <button id="btnStep" class="btn btn-step" onclick="doStep()" disabled>Step</button>{ lv_n }| &&
      |      <button id="btnRun" class="btn btn-run" onclick="doRun()" disabled>Run</button>{ lv_n }| &&
      |      <button id="btnStop" class="btn btn-stop" onclick="doStop()" style="display:none">Stop</button>{ lv_n }| &&
      |      <div class="timeout-control" id="timeoutControl" style="display:none">{ lv_n }| &&
      |        <label>Timeout:</label>{ lv_n }| &&
      |        <input type="range" id="timeoutSlider" min="10" max="120" value="60" step="5" oninput="updateTimeout()">{ lv_n }| &&
      |        <span id="timeoutValue">60s</span>{ lv_n }| &&
      |      </div>{ lv_n }| &&
      |    </div>{ lv_n }| &&
      |    <span class="separator">\|</span>{ lv_n }| &&
      |    <div class="toolbar-group">{ lv_n }| &&
      |      <button class="btn btn-export" onclick="doExport('md')">Export</button>{ lv_n }| &&
      |      <button class="btn btn-export" onclick="doExport('map')">Map</button>{ lv_n }| &&
      |    </div>{ lv_n }| &&
      |    <span id="turnInfo"></span>{ lv_n }| &&
      |    <span id="moveCount"></span>{ lv_n }| &&
      |    <span id="envInfo"></span>{ lv_n }| &&
      |  </div>{ lv_n }| &&
      |  <div id="terminal-container"><div id="terminal"></div></div>{ lv_n }| &&
      |  <div id="status">Status: <span id="statusText" class="disconnected">Connecting...</span></div>{ lv_n }| &&
      |  <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.js"></script>{ lv_n }| &&
      |  <script>{ lv_n }| &&
      |    const APC_PATH = "/sap/bc/apc/sap/zork";{ lv_n }| &&
      |    const ENV_FILE = "{ iv_env }";{ lv_n }| &&
      |    const SANITIZE = { lv_sanitize_js };{ lv_n }| &&
      |    let socket = null;{ lv_n }| &&
      |    let currentLine = "";{ lv_n }| &&
      |    let mode = "play";{ lv_n }| &&
      |    let aiTurn = 0;{ lv_n }| &&
      |    let totalMoves = 0;{ lv_n }| &&
      |    let isRunning = false;{ lv_n }| &&
      |    let stopRequested = false;{ lv_n }| &&
      |    let gameOver = false;{ lv_n }| &&
      |    let pendingStep = false;{ lv_n }| &&
      |    let currentUser = "";{ lv_n }| &&
      |    let stepTimeout = 60;{ lv_n }| &&
      |    let stepTimeoutId = null;{ lv_n }| &&
      |    const term = new Terminal(\{| &&
      |      cursorBlink: true,| &&
      |      fontFamily: '"Courier New", Courier, monospace',| &&
      |      fontSize: 16,| &&
      |      cols: 100,| &&
      |      rows: 28,| &&
      |      theme: \{ background: '#000000', foreground: '#00ff00', cursor: '#00ff00' \}| &&
      |    \});{ lv_n }| &&
      |    term.open(document.getElementById('terminal'));{ lv_n }| &&
      |{ lv_n }| &&
      |    function sanitizeText(text) \{| &&
      |      if (!SANITIZE { lv_or } !currentUser) return text;| &&
      |      const re = new RegExp(currentUser, 'gi');| &&
      |      return text.replace(re, '<USER>');| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function updateStatus(text, cls) \{| &&
      |      const el = document.getElementById('statusText');| &&
      |      el.textContent = text;| &&
      |      el.className = cls;| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function updateMoveCount() \{| &&
      |      document.getElementById('moveCount').textContent = totalMoves > 0 ? 'Moves: ' + totalMoves : '';| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function updateTimeout() \{| &&
      |      stepTimeout = parseInt(document.getElementById('timeoutSlider').value);| &&
      |      document.getElementById('timeoutValue').textContent = stepTimeout + 's';| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function updateButtons() \{| &&
      |      document.getElementById('btnPlay').classList.toggle('active', mode === 'play');| &&
      |      document.getElementById('btnWatch').classList.toggle('active', mode === 'watch');| &&
      |      const watching = (mode === 'watch');| &&
      |      const canStep = watching && !isRunning && !gameOver;| &&
      |      document.getElementById('btnStep').disabled = !canStep;| &&
      |      document.getElementById('btnRun').disabled = !canStep;| &&
      |      document.getElementById('btnRun').style.display = isRunning ? 'none' : 'inline-block';| &&
      |      document.getElementById('btnStop').style.display = isRunning ? 'inline-block' : 'none';| &&
      |      document.getElementById('btnRun').classList.toggle('active', isRunning);| &&
      |      if (gameOver) \{| &&
      |        updateStatus('Game Over', 'disconnected');| &&
      |      \} else if (isRunning) \{| &&
      |        updateStatus('AI Running...', 'running');| &&
      |      \} else if (watching) \{| &&
      |        updateStatus('Watching AI', 'watching');| &&
      |      \} else \{| &&
      |        updateStatus('Connected - Playing', 'connected');| &&
      |      \}| &&
      |      document.getElementById('timeoutControl').style.display = watching ? 'flex' : 'none';| &&
      |      if (!watching) document.getElementById('turnInfo').textContent = '';| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function updateTurnInfo(turn, dur, action) \{| &&
      |      aiTurn = parseInt(turn) { lv_or } 0;| &&
      |      let info = 'Turn ' + aiTurn;| &&
      |      if (dur) info += ' \| ' + dur + 's';| &&
      |      if (action) info += ' \| ' + action;| &&
      |      document.getElementById('turnInfo').textContent = info;| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function sendCmd(cmd, val) \{| &&
      |      if (socket && socket.readyState === WebSocket.OPEN) \{| &&
      |        socket.send(JSON.stringify(\{cmd: cmd, value: val { lv_or } ''\}));| &&
      |      \}| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function setMode(m) \{| &&
      |      if (isRunning) doStop();| &&
      |      mode = m;| &&
      |      aiTurn = 0;| &&
      |      gameOver = false;| &&
      |      sendCmd('mode', mode);| &&
      |      updateButtons();| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function doStep() \{| &&
      |      if (mode !== 'watch' { lv_or } isRunning { lv_or } gameOver) return;| &&
      |      pendingStep = true;| &&
      |      sendCmd('step', '1');| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function doRun() \{| &&
      |      if (mode !== 'watch' { lv_or } isRunning { lv_or } gameOver) return;| &&
      |      isRunning = true;| &&
      |      stopRequested = false;| &&
      |      updateButtons();| &&
      |      runNextStep();| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function doStop() \{| &&
      |      if (stepTimeoutId) \{ clearTimeout(stepTimeoutId); stepTimeoutId = null; \}| &&
      |      stopRequested = true;| &&
      |      isRunning = false;| &&
      |      updateButtons();| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function doExport(format) \{| &&
      |      sendCmd('export', format);| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function runNextStep() \{| &&
      |      if (!isRunning { lv_or } stopRequested { lv_or } gameOver) \{| &&
      |        isRunning = false;| &&
      |        updateButtons();| &&
      |        return;| &&
      |      \}| &&
      |      pendingStep = true;| &&
      |      stepTimeoutId = setTimeout(function() \{| &&
      |        term.writeln('\\r\\n\\x1b[31m[TIMEOUT] No response in ' + stepTimeout + 's. Switching to manual.\\x1b[0m');| &&
      |        doStop();| &&
      |      \}, stepTimeout * 1000);| &&
      |      sendCmd('step', '1');| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function onTurnComplete() \{| &&
      |      if (stepTimeoutId) \{ clearTimeout(stepTimeoutId); stepTimeoutId = null; \}| &&
      |      pendingStep = false;| &&
      |      totalMoves++;| &&
      |      updateMoveCount();| &&
      |      if (isRunning && !stopRequested && !gameOver) \{| &&
      |        runNextStep();| &&
      |      \}| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function downloadFile(filename, content) \{| &&
      |      const blob = new Blob([sanitizeText(content)], \{type: 'text/plain'\});| &&
      |      const url = URL.createObjectURL(blob);| &&
      |      const a = document.createElement('a');| &&
      |      a.href = url;| &&
      |      a.download = filename;| &&
      |      document.body.appendChild(a);| &&
      |      a.click();| &&
      |      document.body.removeChild(a);| &&
      |      URL.revokeObjectURL(url);| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function handleMessage(data) \{| &&
      |      if (data.startsWith('\{')) \{| &&
      |        try \{| &&
      |          const msg = JSON.parse(data);| &&
      |          if (msg.type === 'turn') \{| &&
      |            const p = (msg.data { lv_or } '').split(',');| &&
      |            updateTurnInfo(p[0], p[1], p.slice(2).join(','));| &&
      |            onTurnComplete();| &&
      |          \} else if (msg.type === 'mode') \{| &&
      |            mode = msg.data;| &&
      |            updateButtons();| &&
      |          \} else if (msg.type === 'gameover') \{| &&
      |            gameOver = true;| &&
      |            isRunning = false;| &&
      |            updateButtons();| &&
      |          \} else if (msg.type === 'export') \{| &&
      |            downloadFile(msg.filename, msg.content);| &&
      |          \}| &&
      |          return;| &&
      |        \} catch (e) \{ \}| &&
      |      \}| &&
      |      const text = sanitizeText(data);| &&
      |      term.write(text.replace(/\\r?\\n/g, '\\r\\n'));| &&
      |      if (mode === 'play' && data.length > 10) \{| &&
      |        totalMoves++;| &&
      |        updateMoveCount();| &&
      |      \}| &&
      |      if (!currentUser && data.includes('User:')) \{| &&
      |        const m = data.match(/User:\\s*(\\S+)/);| &&
      |        if (m) currentUser = m[1];| &&
      |      \}| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    function connect() \{| &&
      |      const proto = location.protocol === 'https:' ? 'wss:' : 'ws:';| &&
      |      const wsUrl = proto + '//' + location.host + APC_PATH;| &&
      |      term.writeln('\\x1b[33mConnecting to Z-Machine...\\x1b[0m');| &&
      |      updateStatus('Connecting...', 'disconnected');| &&
      |      document.getElementById('envInfo').textContent = '[' + ENV_FILE + ']';| &&
      |      socket = new WebSocket(wsUrl);{ lv_n }| &&
      |      socket.onopen = function() \{| &&
      |        term.writeln('\\x1b[32m[CONNECTED]\\x1b[0m\\r\\n');| &&
      |        updateStatus('Connected - Playing', 'connected');| &&
      |        gameOver = false;| &&
      |        totalMoves = 0;| &&
      |        updateMoveCount();| &&
      |        updateButtons();| &&
      |        if (ENV_FILE !== 'DEFAULT.ENV') sendCmd('env', ENV_FILE);| &&
      |      \};{ lv_n }| &&
      |      socket.onmessage = function(e) \{ handleMessage(e.data); \};{ lv_n }| &&
      |      socket.onclose = function() \{| &&
      |        term.writeln('\\r\\n\\x1b[31m[DISCONNECTED]\\x1b[0m');| &&
      |        updateStatus('Disconnected', 'disconnected');| &&
      |        currentLine = '';| &&
      |        isRunning = false;| &&
      |      \};{ lv_n }| &&
      |      socket.onerror = function() \{| &&
      |        term.writeln('\\r\\n\\x1b[31m[CONNECTION ERROR]\\x1b[0m');| &&
      |        updateStatus('Error', 'disconnected');| &&
      |        isRunning = false;| &&
      |      \};{ lv_n }| &&
      |    \}{ lv_n }| &&
      |{ lv_n }| &&
      |    term.onData(function(data) \{| &&
      |      if (mode !== 'play') return;| &&
      |      if (!socket { lv_or } socket.readyState !== WebSocket.OPEN) return;| &&
      |      const code = data.charCodeAt(0);{ lv_n }| &&
      |      if (code === 13) \{| &&
      |        term.write('\\r\\n');| &&
      |        socket.send(currentLine);| &&
      |        currentLine = '';| &&
      |      \} else if (code === 127 { lv_or } code === 8) \{| &&
      |        if (currentLine.length > 0) \{| &&
      |          currentLine = currentLine.slice(0, -1);| &&
      |          term.write('\\b \\b');| &&
      |        \}| &&
      |      \} else if (code >= 32) \{| &&
      |        currentLine += data;| &&
      |        term.write(data);| &&
      |      \}| &&
      |    \});{ lv_n }| &&
      |{ lv_n }| &&
      |    connect();{ lv_n }| &&
      |  </script>{ lv_n }| &&
      |</body>{ lv_n }| &&
      |</html>|.
  ENDMETHOD.

ENDCLASS.
