# Hason

An uncompliant XML formatter

## TODO

### Whitespace

Whitespace can be meaningful

* `<a>   hello   </a>` vs `<a>hello</a>`

For now I'm stripping all the whitespace around the word

In the future I should look ahead and check if its text or another element.
