Info
----

Revival'n Impact Coop: part 2 (Which is witch?)
- Code Source
- Build instruction

This is the source code of the Amstrad 6128 plus "Which is witch?" demo (part 2 of the Revival'n Impact Coop demo - released in 08/2011).
Apologies about the source structure, I haven't updated it to be more readable, so you get everything I wrote including comments / bug logs / dead or commented code.
This is for educational purposes.

Amstrad cpc rules !
Fkey - 26/08/2016

How to Build
------------

This demo is built using the WinApe Assembler.

Important Note:
The "run Address" keyword in WinApe assembler does a "CALL Address" rather than a JP "Address".
Then, a "run Address" actually pushes the return address in the stack and so corrupts the code you have just built.
In order to overcome that, I added a breakpoint in the "main.asm" file just after the stack pointer initialization to a safer place.
And so, what you have to do to properly build/execute the demo is :
1 - Load the "main.asm" file in the WinApe Assembler
2 - Build and Run it (Using "F9"), you will be blocked at the breakpoint (just after the stack pointer initialization)
3 - Rebuild and Run it (go back to the "main.asm" page in the Assembler and  click "F9"), once again you will be blocked by the breakpoint
4 - From this point, the demo is correctly built, you can just continue the execution and enjoy it.