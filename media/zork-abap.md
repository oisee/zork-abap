**Headline: You Are Standing in an Open Field... Inside SAP GUI.**

_Subtitle: The 46-year reunion of two mainframe legends (and how AI made it happen in one day)._

1979 was a pivotal year for the mainframe.

In Germany, SAP released R/2, the integrated ERP system that would define enterprise computing.

In Massachusetts, Infocom was founded to commercialize Zork, the text adventure born on MIT's PDP-10 mainframes.

They were the twins of the mainframe era.

One went to work (Finance, Materials, Sales).

The other went to play (Dungeons, Grues, Treasures).

For 46 years, they ran on parallel tracks. They never touched.

Until last month.

Microsoft and Activision released the original source code for Zork I. The stars aligned. It was time for a family reunion.

I decided to port the Z-Machine (Infocom's Virtual Machine) into SAP S/4HANA.

The Z-Namespace Prophecy

It turns out, Infocom might have known the future.

In SAP development, every custom object must start with the letter Z.

- The Machine: **Z**-Machine.
    
- The Transaction: `ZORK`.
    
- The Package: `$ZORK`.
    

The game didn't just run. It felt like it was _coming home_.

### **Building a VM in One Day (The "How")**

"Alice," you say, "Writing a Virtual Machine in ABAP takes weeks of reading bit-level specs."

It took one day.

And I didn't write it alone.

I used **VSP (Vibing-Steampunk)** — my open-source bridge that gives **Claude** direct access to the SAP system. I treated the AI not as a chatbot, but as a Senior Developer who loves retro specs.

The "Vibe Coding" Reality Check

Skeptics say AI creates "slop." They say it can't handle strict logic.

Here is what happened when my Z-Machine crashed on the first turn:

**Me:** "It crashes when I try to LOOK. Debug it."

Claude didn't guess. Using VSP, it:

1. **Set a breakpoint** in the ABAP code.
    
2. **Ran the game** inside SAP.
    
3. **Inspected the memory** (finding a Big-Endian vs Little-Endian mismatch in the object table).
    
4. **Fixed the code.**
    

This isn't just generating text. This is **Digital Archaeology** assisted by **Artificial Intelligence**.

### **Why This Matters**

I now have Zork running in S/4HANA.

(See the screenshot below. Yes, that is a text adventure in SAP GUI).

But the real story isn't the game. It's the capability.

If an AI agent can read a 1980s technical spec, navigate a custom ABAP stack, and debug runtime errors to reunite two pieces of 1979 history...

...imagine what it can do for your 2005 legacy code.

The Repo (ZORK-ABAP):

https://github.com/oisee/zork-abap

The Tool (VSP):

https://github.com/oisee/vibing-steampunk

**Credits:**

- **Infocom (1979)** for the Z-Machine.
    
- **SAP (1979)** for the R/2 legacy that led us here.
    
- **Claude (2025)** for writing the code.
    

_P.S. West of House. There is a transport request here._

#SAP #ABAP #Zork #RetroGaming #AI #MCP #DigitalArchaeology #OpenSource #Microsoft