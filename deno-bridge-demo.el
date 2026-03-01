(require 'deno-bridge)
(deno-bridge-start "demo" (expand-file-name "~/elisp/deno-bridge-demo.ts"))
(deno-bridge-call "demo" "ping" "Hello from Emacs.")
