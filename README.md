# ZORK-ABAP

A Z-Machine V3 interpreter written in ABAP. Play classic Infocom text adventures like Zork on SAP systems.

![Vibecoded](https://img.shields.io/badge/vibecoded-with%20Claude%20Code-blueviolet)
![License](https://img.shields.io/badge/license-MIT-green)
![Platform](https://img.shields.io/badge/platform-SAP%20ABAP-blue)

## Status

**Working!** MiniZork runs successfully on SAP systems.

```
West of House
You are standing in an open field west of a white house, with a boarded front door.
There is a small mailbox here.

>
```

## What is This?

This is a complete Z-Machine version 3 interpreter that runs inside SAP. The Z-Machine is the virtual machine created by Infocom in the 1980s to run their text adventure games (Zork, Hitchhiker's Guide to the Galaxy, etc.).

Now you can play these classics during your SAP debugging sessions.

## Vibecoded with Claude Code

This project was **100% vibecoded** using [Claude Code](https://claude.ai/code) - Anthropic's AI coding assistant.

The development workflow used [vibing-steampunk](https://github.com/oisee/vibing-steampunk) (vsp), an MCP server that gives Claude direct access to SAP ADT APIs. This enabled a fully AI-assisted development experience:

- Code written and edited via natural language
- Unit tests run directly on SAP
- Debugging and iteration in real-time
- No manual copy-paste between IDE and SAP

The interpreter was ported from a Python reference implementation (`z3_minimal.py`) through conversational development with Claude.

## Features

- Full Z-Machine V3 instruction set
- ZSCII text encoding/decoding with abbreviations
- Object tree with attributes and properties
- Dictionary lookup and input tokenization
- Interactive HTML console (24-line retro display)
- Automated speedrun/test mode with assertions
- Load games from SMW0 or filesystem

## Architecture

```
zcl_ork_00_zmachine     - Main interpreter (fetch-decode-execute)
zcl_ork_00_memory       - Memory management (big-endian)
zcl_ork_00_stack        - Call frames and evaluation stack
zcl_ork_00_objects      - Object table, attributes, properties
zcl_ork_00_text         - ZSCII text decoder
zcl_ork_00_dict         - Dictionary and tokenization
```

## Running

### Interactive Console

```
SE38 -> ZORK_00_CONSOLE
```

Select a game from SMW0 or browse for a local `.z3` file.

### Speedrun/Test Mode

```
SE38 -> ZORK_00_SPEEDRUN
```

Runs automated command scripts with verification assertions.

## Installation

1. Create package `$ZORK_00` in your SAP system
2. Import the ABAP classes via abapGit or ADT
3. Upload game files (`.z3`) to SMW0
4. Run `ZORK_00_CONSOLE`

## Development

This project uses [vibing-steampunk](https://github.com/oisee/vibing-steampunk) for AI-assisted development:

```json
{
  "mcpServers": {
    "a4h-abap-adt": {
      "command": "/path/to/vsp",
      "env": {
        "SAP_URL": "http://your-sap-host:50000",
        "SAP_USER": "your-user",
        "SAP_PASSWORD": "your-password"
      }
    }
  }
}
```

See [CLAUDE.md](CLAUDE.md) for detailed development guidelines.

## Testing

Run unit tests via MCP:

```
RunUnitTests(object_url="/sap/bc/adt/oo/classes/ZCL_ORK_00_SPEEDRUN")
```

Or in SAP GUI: `SE38 -> ZORK_00_SPEEDRUN` with test scripts.

## Clean-Room Implementation

This is a **clean-room implementation** based on the publicly available Z-Machine specification:

- Implemented from the [Z-Machine Standards Document 1.1](https://www.inform-fiction.org/zmachine/standards/z1point1/) by Graham Nelson
- The Z-Machine format and specification are **public domain** / freely available
- No code was copied - the ABAP implementation is original work
- Architecture inspired by other Z-Machine implementations for reference

## References

### Z-Machine Specification
- [Z-Machine Standards Document 1.1](https://www.inform-fiction.org/zmachine/standards/z1point1/) - The definitive spec by Graham Nelson

### Inspiration
- [zmachine](https://github.com/ravdin/zmachine) - Python implementation that inspired the architecture
- [xyppy](https://github.com/theinternetftw/xyppy) - Python Z-Machine used as reference

### Development Tools
- [vibing-steampunk](https://github.com/oisee/vibing-steampunk) - MCP server for SAP ADT
- [Claude Code](https://claude.ai/code) - AI coding assistant

## License

MIT License - see [LICENSE](LICENSE) file.

The Z-Machine specification itself is public domain. Game story files (`.z3`, `.z5`, etc.) may have their own licensing terms.

## Credits

- **Graham Nelson** - Z-Machine Standards Document
- **Infocom** - Original Z-Machine design and games
- **[ravdin/zmachine](https://github.com/ravdin/zmachine)** - Python implementation (architecture inspiration)
- **Anthropic** - Claude Code, the AI that wrote this
- **vibing-steampunk** - MCP bridge that made it possible

---

*"It is pitch black. You are likely to be eaten by a grue."*
