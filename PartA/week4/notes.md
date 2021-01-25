Lexical scope: use environment where function is defined.
Dynamic scope: use environment where function is called.

Decades ago, both might have been considered reasonable, but now know lexical scope makes much more sense.

## Does dynamic scope exist?

- Lexical scope for variables is definitely the right default
    - Very common across languages

- Dynamic scope is occasionally convinient in some situations
    - So some languages (e.g., Racket) have special ways to do it
    - But most do not bother

- If you squint some, exception handling is more like dynamic scope:
    - raise e transfers control to the current innermost handler
    - Does not have to be syntactically inside a handle expression (and usually is not)
