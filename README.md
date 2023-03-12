Example Haskell Web App

Both our Maskies video game and our Ged2Dendro productivity app will benefit from an implementation as a web application. To this effect we needed to find a haskell-based web server. This project serves to perform research and development.

For now we have settled on the WARP web server, which implements WAI as a general interface for web applications. We have tried several others. They've been rejected due to problems like lack of documentation, lack of examples, archived and outdated packages, packages that aren't maintained anymore, packages that won't compile anymore, etc. 

WARP and WAI by themselves don't use a lot of dependencies. But WAI-Extra does. It's a pretty necessary addition for things like delivering GZipped content, logging, and JSON handling.

Feel free to copy the source and use it to your benefit.
