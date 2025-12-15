# ZORK WebSocket Terminal - APC Architecture

This document describes the WebSocket-based terminal implementation for running ZORK (Z-Machine) on SAP using ABAP Push Channels (APC).

## Overview

The solution provides a browser-based terminal that connects via WebSocket to an ABAP backend running the Z-Machine interpreter. The architecture leverages SAP's APC framework for real-time bidirectional communication.

```
┌─────────────┐     WebSocket      ┌─────────────┐     ┌─────────────┐
│   Browser   │◄──────────────────►│     ICM     │◄───►│ Work Process│
│  (xterm.js) │                    │             │     │             │
└─────────────┘                    └─────────────┘     └─────────────┘
                                          ▲
                                          │
                                   ┌──────┴──────┐
                                   │   Shared    │
                                   │   Memory    │
                                   │  (Session)  │
                                   └─────────────┘
```

## Components

### ABAP Classes (Package: $ZORK_02)

| Class | Purpose |
|-------|---------|
| `ZCL_ORK_02_APC` | Stateful WebSocket handler integrating Z-Machine |
| `ZCL_ORK_02_HTTP` | HTTP handler serving the xterm.js terminal HTML |

### SAP Configuration

| Object | Path/Name |
|--------|-----------|
| APC Application | `ZORK` (Transaction SAPC) |
| APC SICF Service | `/sap/bc/apc/sap/zork` |
| HTML SICF Service | `/sap/bc/zork` |

## Stateful APC Architecture

### How State Persistence Works

The APC framework automatically handles session state between WebSocket messages:

1. **ON_START** (connection opens)
   - Work process acquired briefly
   - Handler instance created, Z-Machine initialized
   - State serialized to shared memory
   - Work process released

2. **Waiting for input**
   - No work process held
   - Session context preserved in shared memory
   - ICM maintains WebSocket connection

3. **ON_MESSAGE** (user sends command)
   - Work process acquired
   - Session state deserialized (including Z-Machine state)
   - Command processed
   - State re-serialized
   - Work process released

4. **ON_CLOSE** (connection ends)
   - Session cleaned up
   - Resources freed

### Key Insight

The entire class instance (including `mo_zmachine` with all its memory, stack, program counter, etc.) is automatically serialized/deserialized by the APC framework. This is transparent to our code.

### Resource Efficiency

- Work processes are only occupied during actual message processing
- Many concurrent WebSocket connections can share few work processes
- Sessions are "warm" - no re-initialization between messages
- SM66 won't show persistent processes (they're released between messages)

## Session Lifecycle

```
Page Load → HTTP Request → HTML/JS returned
    │
    └──► JavaScript connects WebSocket
              │
              ▼
         ON_ACCEPT → Accept connection
              │
              ▼
         ON_START → Initialize Z-Machine, send welcome
              │
              ▼
         ┌───────────────────────────┐
         │  Waiting for user input   │◄─────────┐
         │  (no work process held)   │          │
         └───────────┬───────────────┘          │
                     │                          │
                     ▼                          │
              ON_MESSAGE                        │
                     │                          │
                     ▼                          │
         Process command, send output ──────────┘
              │
              ▼
         ON_CLOSE (browser closed/refresh)
              │
              ▼
         Session ended, state lost
```

## Important Behavior

### Page Refresh = New Session

Refreshing the browser page will:
1. Close the existing WebSocket connection (ON_CLOSE triggered)
2. Reload HTML page
3. Establish new WebSocket connection (ON_START triggered)
4. Initialize fresh Z-Machine (game progress lost)

### Session Persistence (Optional Enhancement)

To survive page refresh, implement state persistence:

```abap
METHOD if_apc_wsp_extension~on_start.
  " Check for existing session token
  DATA(lv_token) = get_session_token( i_context ).

  IF lv_token IS NOT INITIAL.
    " Restore existing game state
    IMPORT zmachine = mo_zmachine
      FROM DATABASE zork_sessions(zk) ID lv_token.
  ELSE.
    " New session - create fresh Z-Machine
    mo_zmachine = NEW zcl_ork_00_zmachine( load_story( ) ).
    lv_token = create_new_token( ).
  ENDIF.
ENDMETHOD.

METHOD if_apc_wsp_extension~on_close.
  " Save state for potential reconnection
  IF mv_session_token IS NOT INITIAL.
    EXPORT zmachine = mo_zmachine
      TO DATABASE zork_sessions(zk) ID mv_session_token.
  ENDIF.
ENDMETHOD.
```

## Connection Monitoring

### Shared Memory Counter

The handler tracks active connections using shared memory:

```abap
" Increment on connect
IMPORT count = lv_count FROM SHARED MEMORY indx(zk) ID 'ZORK_APC_CONNECTIONS'.
lv_count = lv_count + 1.
EXPORT count = lv_count TO SHARED MEMORY indx(zk) ID 'ZORK_APC_CONNECTIONS'.

" Check count programmatically
DATA(lv_active) = zcl_ork_02_apc=>get_connection_count( ).
```

### Why SM66 Doesn't Show Connections

APC stateful connections don't hold work processes permanently. The work process is only used during actual message processing, then released. Between messages, the session context lives in shared memory, not in a work process.

## Use Cases Beyond Gaming

This architecture pattern is ideal for:

| Use Case | Benefit |
|----------|---------|
| **Screening Services** | Pre-load strategies on connect, instant inference |
| **Interactive Reports** | Maintain report context between drill-downs |
| **Chat/Chatbot** | Preserve conversation context |
| **Monitoring Dashboards** | Push real-time updates to browser |
| **Collaborative Tools** | Real-time synchronization |

### Example: Warmed-Up Screening Service

```abap
METHOD if_apc_wsp_extension~on_start.
  " Warm up - load all strategies and models once
  mo_screening_engine = NEW zcl_screening_engine( ).
  mo_screening_engine->load_strategies( ).
  mo_screening_engine->initialize_ml_models( ).
  mo_screening_engine->cache_reference_data( ).
  " Now ready for instant screening requests
ENDMETHOD.

METHOD if_apc_wsp_extension~on_message.
  " Instant screening - engine already warmed up
  DATA(lv_request) = i_message->get_text( ).
  DATA(lv_result) = mo_screening_engine->screen( lv_request ).
  send_response( lv_result ).
ENDMETHOD.
```

## Setup Instructions

### 1. Create APC Application (Transaction SAPC)

| Field | Value |
|-------|-------|
| Application | `ZORK` |
| Description | `ZORK Z-Machine WebSocket Terminal` |
| Handler Class | `ZCL_ORK_02_APC` |
| Connection Type | WebSocket |
| Stateful | **Yes** (checked) |

### 2. Activate APC SICF Service

Path: `/sap/bc/apc/sap/zork`

### 3. Create HTTP SICF Service (Transaction SICF)

| Field | Value |
|-------|-------|
| Path | `/sap/bc/zork` |
| Handler | `ZCL_ORK_02_HTTP` |

### 4. Test

Open in browser: `http://<host>:<port>/sap/bc/zork`

## Dependencies

- Z-Machine interpreter classes from `$ZORK_00` package
- Game file `ZORK-MINI.Z3` in SMW0 (MIME Repository)
- xterm.js loaded from CDN (jsdelivr.net)

## APC vs OData: State Management Comparison

### OData is Fundamentally Stateless

OData follows REST principles - each request is independent. SAP provides mechanisms for state-like behavior, but they work differently from APC.

### State Mechanisms Comparison

| Aspect | APC Stateful | OData (RAP Draft) | OData (SEGW Stateful) |
|--------|--------------|-------------------|----------------------|
| State location | Shared memory (serialized) | Database tables | Work process memory |
| Work process | Released between messages | Released between requests | **Held permanently** |
| Persistence | Session lifetime only | Survives restarts | Session lifetime |
| Latency | Lower (memory) | Higher (DB I/O) | Lowest |
| Scalability | Good | Better | **Poor** |
| Cloud ABAP support | **No** | **Yes** | No |

### How RAP Draft Works

State survives in **database draft tables**, not in memory:

```
┌─────────────┐     HTTP      ┌─────────────┐     ┌─────────────┐
│   Browser   │──────────────►│ Work Process│────►│ Draft Table │
│             │◄──────────────│  (stateless)│◄────│  (DB state) │
└─────────────┘               └─────────────┘     └─────────────┘
```

```abap
" RAP Behavior Definition
define behavior for ZI_SalesOrder
  with draft;

" Draft is persisted to:
" - ZSALESORDER_D (draft data)
" - ZSALESORDER_X (draft admin)
```

**Each request:**
1. Read draft from DB
2. Process changes
3. Write draft back to DB
4. Work process released (no memory state)

### SEGW Stateful Sessions (Legacy - Not Recommended)

Older SAP Gateway could use stateful sessions:

```abap
" In SEGW service implementation
METHOD /iwbep/if_mgw_appl_srv_runtime~set_header.
  /iwfnd/cl_sutil_moni=>set_stateful_session( abap_true ).
ENDMETHOD.
```

**Warning:** This holds a work process permanently - not scalable!

### Cloud ABAP (Steampunk) Considerations

APC is **NOT available** in SAP BTP ABAP Environment (Steampunk). Only "released APIs" are available, and APC classes are not on the list.

**For ZORK on Cloud ABAP, alternatives:**

1. **HTTP Polling with DB State**
   ```
   Browser ──HTTP GET /poll──► RAP Service ──► Z-Machine (from DB) ──► Response
   ```

2. **OData with Serialized State**
   ```abap
   METHOD process_command.
     " Load Z-Machine from DB
     IMPORT zmachine = mo_zmachine
       FROM DATABASE zork_saves(zk) ID iv_session_id.

     " Process command
     mo_zmachine->provide_input( iv_command ).
     mo_zmachine->run( ).
     DATA(lv_output) = mo_zmachine->get_status( )-output.

     " Save state back to DB
     EXPORT zmachine = mo_zmachine
       TO DATABASE zork_saves(zk) ID iv_session_id.

     " Return output
     rv_output = lv_output.
   ENDMETHOD.
   ```

3. **External WebSocket Gateway**
   ```
   Browser ──WebSocket──► CF Node.js ──HTTP──► Steampunk RAP
   ```

### Trade-offs: APC vs DB-based State

| Scenario | Best Choice | Reason |
|----------|-------------|--------|
| Real-time interactive (gaming, chat) | APC | Low latency, bidirectional |
| Cloud ABAP / Steampunk | OData + DB | Only option available |
| Long-running drafts | RAP Draft | Survives restarts |
| High concurrency | OData + DB | Better scalability |
| Screening service (warmed up) | APC | Pre-loaded models in memory |

## Security Considerations

- Authentication handled by standard SAP ICM/ICF
- Same-origin policy: HTML must be served from same SAP system
- Session cookies shared between HTTP and WebSocket endpoints
- Consider adding CSRF protection for production use
