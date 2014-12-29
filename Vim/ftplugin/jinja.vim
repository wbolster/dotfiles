" Vim configuration for Jinja2 templates

setlocal tabstop=2 shiftwidth=2

" Extra surroundings for surround plugin (surround.vim)
let b:surround_{char2nr("v")} = "{{ \r }}"
let b:surround_{char2nr("{")} = "{{ \r }}"
let b:surround_{char2nr("%")} = "{% \r %}"
let b:surround_{char2nr("i")} = "{% if \1condition: \1 %}\r{% endif %}"
let b:surround_{char2nr("f")} = "{% for \1for loop: \1 %}\r{% endfor %}"
let b:surround_{char2nr("c")} = "{# \r #}"
let b:surround_{char2nr("#")} = "{# \r #}"
