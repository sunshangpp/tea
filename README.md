# tea

Building interactive shell applications with the elm architecture (<https://guide.elm-lang.org/architecture/>)

Inspired by:

- <https://github.com/charmbracelet/bubbletea>
- <https://github.com/neochrome/teash>

Built on top of:

- <https://github.com/pqwy/notty>
- <https://github.com/janestreet/async>

## Try the examples

Examples are located in the `/examples` directory.

Try it out:

```sh
# first clone the codebase, then

make create_switch
make switch

dune exec examples/select.exe # or spinner.exe, progress.exe etc
```
